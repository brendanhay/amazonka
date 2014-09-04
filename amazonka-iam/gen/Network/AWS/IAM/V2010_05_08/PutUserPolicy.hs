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
    , mkPutUserPolicyRequest
    -- ** Request lenses
    , puprUserName
    , puprPolicyName
    , puprPolicyDocument

    -- * Response
    , PutUserPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutUserPolicy' request.
mkPutUserPolicyRequest :: Text -- ^ 'puprUserName'
                       -> Text -- ^ 'puprPolicyName'
                       -> Text -- ^ 'puprPolicyDocument'
                       -> PutUserPolicy
mkPutUserPolicyRequest p1 p2 p3 = PutUserPolicy
    { _puprUserName = p1
    , _puprPolicyName = p2
    , _puprPolicyDocument = p3
    }
{-# INLINE mkPutUserPolicyRequest #-}

data PutUserPolicy = PutUserPolicy
    { _puprUserName :: Text
      -- ^ Name of the user to associate the policy with.
    , _puprPolicyName :: Text
      -- ^ Name of the policy document.
    , _puprPolicyDocument :: Text
      -- ^ The policy document.
    } deriving (Show, Generic)

-- | Name of the user to associate the policy with.
puprUserName :: Lens' PutUserPolicy (Text)
puprUserName = lens _puprUserName (\s a -> s { _puprUserName = a })
{-# INLINE puprUserName #-}

-- | Name of the policy document.
puprPolicyName :: Lens' PutUserPolicy (Text)
puprPolicyName = lens _puprPolicyName (\s a -> s { _puprPolicyName = a })
{-# INLINE puprPolicyName #-}

-- | The policy document.
puprPolicyDocument :: Lens' PutUserPolicy (Text)
puprPolicyDocument = lens _puprPolicyDocument (\s a -> s { _puprPolicyDocument = a })
{-# INLINE puprPolicyDocument #-}

instance ToQuery PutUserPolicy where
    toQuery = genericQuery def

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutUserPolicy where
    type Sv PutUserPolicy = IAM
    type Rs PutUserPolicy = PutUserPolicyResponse

    request = post "PutUserPolicy"
    response _ = nullaryResponse PutUserPolicyResponse
