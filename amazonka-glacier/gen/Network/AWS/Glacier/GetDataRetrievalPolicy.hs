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

-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
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

-- | <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetDataRetrievalPolicy.html>
module Network.AWS.Glacier.GetDataRetrievalPolicy
    (
    -- * Request
      GetDataRetrievalPolicy
    -- ** Request constructor
    , getDataRetrievalPolicy
    -- ** Request lenses
    , gdrpAccountId

    -- * Response
    , GetDataRetrievalPolicyResponse
    -- ** Response constructor
    , getDataRetrievalPolicyResponse
    -- ** Response lenses
    , gdrprPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

newtype GetDataRetrievalPolicy = GetDataRetrievalPolicy
    { _gdrpAccountId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetDataRetrievalPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrpAccountId' @::@ 'Text'
--
getDataRetrievalPolicy :: Text -- ^ 'gdrpAccountId'
                       -> GetDataRetrievalPolicy
getDataRetrievalPolicy p1 = GetDataRetrievalPolicy
    { _gdrpAccountId = p1
    }

gdrpAccountId :: Lens' GetDataRetrievalPolicy Text
gdrpAccountId = lens _gdrpAccountId (\s a -> s { _gdrpAccountId = a })

newtype GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse
    { _gdrprPolicy :: Maybe DataRetrievalPolicy
    } deriving (Eq, Read, Show)

-- | 'GetDataRetrievalPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrprPolicy' @::@ 'Maybe' 'DataRetrievalPolicy'
--
getDataRetrievalPolicyResponse :: GetDataRetrievalPolicyResponse
getDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse
    { _gdrprPolicy = Nothing
    }

gdrprPolicy :: Lens' GetDataRetrievalPolicyResponse (Maybe DataRetrievalPolicy)
gdrprPolicy = lens _gdrprPolicy (\s a -> s { _gdrprPolicy = a })

instance ToPath GetDataRetrievalPolicy where
    toPath GetDataRetrievalPolicy{..} = mconcat
        [ "/"
        , toText _gdrpAccountId
        , "/policies/data-retrieval"
        ]

instance ToQuery GetDataRetrievalPolicy where
    toQuery = const mempty

instance ToHeaders GetDataRetrievalPolicy

instance ToJSON GetDataRetrievalPolicy where
    toJSON = const (toJSON Empty)

instance AWSRequest GetDataRetrievalPolicy where
    type Sv GetDataRetrievalPolicy = Glacier
    type Rs GetDataRetrievalPolicy = GetDataRetrievalPolicyResponse

    request  = get
    response = jsonResponse

instance FromJSON GetDataRetrievalPolicyResponse where
    parseJSON = withObject "GetDataRetrievalPolicyResponse" $ \o -> GetDataRetrievalPolicyResponse
        <$> o .:? "Policy"
