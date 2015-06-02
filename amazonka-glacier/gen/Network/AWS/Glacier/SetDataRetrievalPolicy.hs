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

-- Module      : Network.AWS.Glacier.SetDataRetrievalPolicy
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

-- | This operation sets and then enacts a data retrieval policy in the region
-- specified in the PUT request. You can set one policy per region for an AWS
-- account. The policy is enacted within a few minutes of a successful PUT
-- operation.
--
-- The set policy operation does not affect retrieval jobs that were in
-- progress before the policy was enacted. For more information about data
-- retrieval policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetDataRetrievalPolicy.html>
module Network.AWS.Glacier.SetDataRetrievalPolicy
    (
    -- * Request
      SetDataRetrievalPolicy
    -- ** Request constructor
    , setDataRetrievalPolicy
    -- ** Request lenses
    , sdrpAccountId
    , sdrpPolicy

    -- * Response
    , SetDataRetrievalPolicyResponse
    -- ** Response constructor
    , setDataRetrievalPolicyResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data SetDataRetrievalPolicy = SetDataRetrievalPolicy
    { _sdrpAccountId :: Text
    , _sdrpPolicy    :: Maybe DataRetrievalPolicy
    } deriving (Eq, Read, Show)

-- | 'SetDataRetrievalPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdrpAccountId' @::@ 'Text'
--
-- * 'sdrpPolicy' @::@ 'Maybe' 'DataRetrievalPolicy'
--
setDataRetrievalPolicy :: Text -- ^ 'sdrpAccountId'
                       -> SetDataRetrievalPolicy
setDataRetrievalPolicy p1 = SetDataRetrievalPolicy
    { _sdrpAccountId = p1
    , _sdrpPolicy    = Nothing
    }

-- | The 'AccountId' value is the AWS account ID. This value must match the AWS
-- account ID associated with the credentials used to sign the request. You can
-- either specify an AWS account ID or optionally a single apos'-'apos (hyphen),
-- in which case Amazon Glacier uses the AWS account ID associated with the
-- credentials used to sign the request. If you specify your Account ID, do not
-- include any hyphens (apos-apos) in the ID.
sdrpAccountId :: Lens' SetDataRetrievalPolicy Text
sdrpAccountId = lens _sdrpAccountId (\s a -> s { _sdrpAccountId = a })

-- | The data retrieval policy in JSON format.
sdrpPolicy :: Lens' SetDataRetrievalPolicy (Maybe DataRetrievalPolicy)
sdrpPolicy = lens _sdrpPolicy (\s a -> s { _sdrpPolicy = a })

data SetDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetDataRetrievalPolicyResponse' constructor.
setDataRetrievalPolicyResponse :: SetDataRetrievalPolicyResponse
setDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse

instance ToPath SetDataRetrievalPolicy where
    toPath SetDataRetrievalPolicy{..} = mconcat
        [ "/"
        , toText _sdrpAccountId
        , "/policies/data-retrieval"
        ]

instance ToQuery SetDataRetrievalPolicy where
    toQuery = const mempty

instance ToHeaders SetDataRetrievalPolicy

instance ToJSON SetDataRetrievalPolicy where
    toJSON SetDataRetrievalPolicy{..} = object
        [ "Policy" .= _sdrpPolicy
        ]

instance AWSRequest SetDataRetrievalPolicy where
    type Sv SetDataRetrievalPolicy = Glacier
    type Rs SetDataRetrievalPolicy = SetDataRetrievalPolicyResponse

    request  = put
    response = nullResponse SetDataRetrievalPolicyResponse
