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

-- Module      : Network.AWS.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves a policy attached to the specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyPolicy.html>
module Network.AWS.KMS.GetKeyPolicy
    (
    -- * Request
      GetKeyPolicy
    -- ** Request constructor
    , getKeyPolicy
    -- ** Request lenses
    , gkpKeyId
    , gkpPolicyName

    -- * Response
    , GetKeyPolicyResponse
    -- ** Response constructor
    , getKeyPolicyResponse
    -- ** Response lenses
    , gkprPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data GetKeyPolicy = GetKeyPolicy
    { _gkpKeyId      :: Text
    , _gkpPolicyName :: Text
    } deriving (Eq, Ord, Show)

-- | 'GetKeyPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkpKeyId' @::@ 'Text'
--
-- * 'gkpPolicyName' @::@ 'Text'
--
getKeyPolicy :: Text -- ^ 'gkpKeyId'
             -> Text -- ^ 'gkpPolicyName'
             -> GetKeyPolicy
getKeyPolicy p1 p2 = GetKeyPolicy
    { _gkpKeyId      = p1
    , _gkpPolicyName = p2
    }

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
gkpKeyId :: Lens' GetKeyPolicy Text
gkpKeyId = lens _gkpKeyId (\s a -> s { _gkpKeyId = a })

-- | String that contains the name of the policy. Currently, this must be
-- "default". Policy names can be discovered by calling ListKeyPolicies.
gkpPolicyName :: Lens' GetKeyPolicy Text
gkpPolicyName = lens _gkpPolicyName (\s a -> s { _gkpPolicyName = a })

newtype GetKeyPolicyResponse = GetKeyPolicyResponse
    { _gkprPolicy :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'GetKeyPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkprPolicy' @::@ 'Maybe' 'Text'
--
getKeyPolicyResponse :: GetKeyPolicyResponse
getKeyPolicyResponse = GetKeyPolicyResponse
    { _gkprPolicy = Nothing
    }

-- | A policy document in JSON format.
gkprPolicy :: Lens' GetKeyPolicyResponse (Maybe Text)
gkprPolicy = lens _gkprPolicy (\s a -> s { _gkprPolicy = a })

instance ToPath GetKeyPolicy where
    toPath = const "/"

instance ToQuery GetKeyPolicy where
    toQuery = const mempty

instance ToHeaders GetKeyPolicy

instance ToJSON GetKeyPolicy where
    toJSON GetKeyPolicy{..} = object
        [ "KeyId"      .= _gkpKeyId
        , "PolicyName" .= _gkpPolicyName
        ]

json

instance AWSRequest GetKeyPolicy where
    type Sv GetKeyPolicy = KMS
    type Rs GetKeyPolicy = GetKeyPolicyResponse

    request  = post "GetKeyPolicy"
    response = jsonResponse

instance FromJSON GetKeyPolicyResponse where
    parseJSON = withObject "GetKeyPolicyResponse" $ \o -> GetKeyPolicyResponse
        <$> o .:? "Policy"
