{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.PutUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified user. For
-- information about policies, refer to Overview of Policies in the Using IAM
-- guide. For information about limits on the number of policies you can
-- associate with a user, see Limitations on IAM Entities in the Using IAM
-- guide. Because policy documents can be large, you should use POST rather
-- than GET when calling PutUserPolicy. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.PutUserPolicy
    (
    -- * Request
      PutUserPolicy
    -- ** Request constructor
    , putUserPolicy
    -- ** Request lenses
    , puprUserName
    , puprPolicyDocument
    , puprPolicyName

    -- * Response
    , PutUserPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutUserPolicy' request.
putUserPolicy :: Text -- ^ 'puprUserName'
              -> Text -- ^ 'puprPolicyDocument'
              -> Text -- ^ 'puprPolicyName'
              -> PutUserPolicy
putUserPolicy p1 p2 p3 = PutUserPolicy
    { _puprUserName = p1
    , _puprPolicyDocument = p2
    , _puprPolicyName = p3
    }

data PutUserPolicy = PutUserPolicy
    { _puprUserName :: Text
      -- ^ Name of the user to associate the policy with.
    , _puprPolicyDocument :: Text
      -- ^ The policy document.
    , _puprPolicyName :: Text
      -- ^ Name of the policy document.
    } deriving (Show, Generic)

-- | Name of the user to associate the policy with.
puprUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutUserPolicy
    -> f PutUserPolicy
puprUserName f x =
    (\y -> x { _puprUserName = y })
       <$> f (_puprUserName x)
{-# INLINE puprUserName #-}

-- | The policy document.
puprPolicyDocument
    :: Functor f
    => (Text
    -> f (Text))
    -> PutUserPolicy
    -> f PutUserPolicy
puprPolicyDocument f x =
    (\y -> x { _puprPolicyDocument = y })
       <$> f (_puprPolicyDocument x)
{-# INLINE puprPolicyDocument #-}

-- | Name of the policy document.
puprPolicyName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutUserPolicy
    -> f PutUserPolicy
puprPolicyName f x =
    (\y -> x { _puprPolicyName = y })
       <$> f (_puprPolicyName x)
{-# INLINE puprPolicyName #-}

instance ToQuery PutUserPolicy where
    toQuery = genericQuery def

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutUserPolicy where
    type Sv PutUserPolicy = IAM
    type Rs PutUserPolicy = PutUserPolicyResponse

    request = post "PutUserPolicy"
    response _ = nullaryResponse PutUserPolicyResponse
