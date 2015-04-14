{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.GetPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the access policy, containing a list of permissions granted via the 'AddPermission' API, associated with the specified bucket.
--
-- You need permission for the 'lambda:GetPolicy action.'
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_GetPolicy.html>
module Network.AWS.Lambda.GetPolicy
    (
    -- * Request
      GetPolicy
    -- ** Request constructor
    , getPolicy
    -- ** Request lenses
    , gpFunctionName

    -- * Response
    , GetPolicyResponse
    -- ** Response constructor
    , getPolicyResponse
    -- ** Response lenses
    , gprPolicy
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype GetPolicy = GetPolicy
    { _gpFunctionName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpFunctionName' @::@ 'Text'
--
getPolicy :: Text -- ^ 'gpFunctionName'
          -> GetPolicy
getPolicy p1 = GetPolicy
    { _gpFunctionName = p1
    }

-- | Function name whose access policy you want to retrieve.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
gpFunctionName :: Lens' GetPolicy Text
gpFunctionName = lens _gpFunctionName (\s a -> s { _gpFunctionName = a })

newtype GetPolicyResponse = GetPolicyResponse
    { _gprPolicy :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'GetPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprPolicy' @::@ 'Maybe' 'Text'
--
getPolicyResponse :: GetPolicyResponse
getPolicyResponse = GetPolicyResponse
    { _gprPolicy = Nothing
    }

-- | The access policy associated with the specified function. The response
-- returns the same as a string using "\" as an escape character in the JSON.
gprPolicy :: Lens' GetPolicyResponse (Maybe Text)
gprPolicy = lens _gprPolicy (\s a -> s { _gprPolicy = a })

instance ToPath GetPolicy where
    toPath GetPolicy{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _gpFunctionName
        , "/versions/HEAD/policy"
        ]

instance ToQuery GetPolicy where
    toQuery = const mempty

instance ToHeaders GetPolicy

instance ToJSON GetPolicy where
    toJSON = const (toJSON Empty)

instance AWSRequest GetPolicy where
    type Sv GetPolicy = Lambda
    type Rs GetPolicy = GetPolicyResponse

    request  = get
    response = jsonResponse

instance FromJSON GetPolicyResponse where
    parseJSON = withObject "GetPolicyResponse" $ \o -> GetPolicyResponse
        <$> o .:? "Policy"
