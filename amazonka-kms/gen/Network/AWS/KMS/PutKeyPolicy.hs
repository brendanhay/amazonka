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

-- Module      : Network.AWS.KMS.PutKeyPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches a policy to the specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_PutKeyPolicy.html>
module Network.AWS.KMS.PutKeyPolicy
    (
    -- * Request
      PutKeyPolicy
    -- ** Request constructor
    , putKeyPolicy
    -- ** Request lenses
    , pkpKeyId
    , pkpPolicy
    , pkpPolicyName

    -- * Response
    , PutKeyPolicyResponse
    -- ** Response constructor
    , putKeyPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data PutKeyPolicy = PutKeyPolicy
    { _pkpKeyId      :: Text
    , _pkpPolicy     :: Text
    , _pkpPolicyName :: Text
    } deriving (Eq, Ord, Show)

-- | 'PutKeyPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pkpKeyId' @::@ 'Text'
--
-- * 'pkpPolicy' @::@ 'Text'
--
-- * 'pkpPolicyName' @::@ 'Text'
--
putKeyPolicy :: Text -- ^ 'pkpKeyId'
             -> Text -- ^ 'pkpPolicyName'
             -> Text -- ^ 'pkpPolicy'
             -> PutKeyPolicy
putKeyPolicy p1 p2 p3 = PutKeyPolicy
    { _pkpKeyId      = p1
    , _pkpPolicyName = p2
    , _pkpPolicy     = p3
    }

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
pkpKeyId :: Lens' PutKeyPolicy Text
pkpKeyId = lens _pkpKeyId (\s a -> s { _pkpKeyId = a })

-- | The policy, in JSON format, to be attached to the key.
pkpPolicy :: Lens' PutKeyPolicy Text
pkpPolicy = lens _pkpPolicy (\s a -> s { _pkpPolicy = a })

-- | Name of the policy to be attached. Currently, the only supported name is
-- "default".
pkpPolicyName :: Lens' PutKeyPolicy Text
pkpPolicyName = lens _pkpPolicyName (\s a -> s { _pkpPolicyName = a })

data PutKeyPolicyResponse = PutKeyPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutKeyPolicyResponse' constructor.
putKeyPolicyResponse :: PutKeyPolicyResponse
putKeyPolicyResponse = PutKeyPolicyResponse

instance ToPath PutKeyPolicy where
    toPath = const "/"

instance ToQuery PutKeyPolicy where
    toQuery = const mempty

instance ToHeaders PutKeyPolicy

instance ToJSON PutKeyPolicy where
    toJSON PutKeyPolicy{..} = object
        [ "KeyId"      .= _pkpKeyId
        , "PolicyName" .= _pkpPolicyName
        , "Policy"     .= _pkpPolicy
        ]

instance AWSRequest PutKeyPolicy where
    type Sv PutKeyPolicy = KMS
    type Rs PutKeyPolicy = PutKeyPolicyResponse

    request  = post "PutKeyPolicy"
    response = nullResponse PutKeyPolicyResponse


Some kind of operator / class to check the types whether to continue?
