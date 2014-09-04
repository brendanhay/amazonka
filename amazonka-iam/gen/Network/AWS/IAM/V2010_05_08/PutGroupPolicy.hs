{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.PutGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified group.
-- For information about policies, refer to Overview of Policies in the Using
-- IAM guide. For information about limits on the number of policies you can
-- associate with a group, see Limitations on IAM Entities in the Using IAM
-- guide. Because policy documents can be large, you should use POST rather
-- than GET when calling PutGroupPolicy. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.PutGroupPolicy
    (
    -- * Request
      PutGroupPolicy
    -- ** Request constructor
    , mkPutGroupPolicyRequest
    -- ** Request lenses
    , pgprGroupName
    , pgprPolicyName
    , pgprPolicyDocument

    -- * Response
    , PutGroupPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutGroupPolicy' request.
mkPutGroupPolicyRequest :: Text -- ^ 'pgprGroupName'
                        -> Text -- ^ 'pgprPolicyName'
                        -> Text -- ^ 'pgprPolicyDocument'
                        -> PutGroupPolicy
mkPutGroupPolicyRequest p1 p2 p3 = PutGroupPolicy
    { _pgprGroupName = p1
    , _pgprPolicyName = p2
    , _pgprPolicyDocument = p3
    }
{-# INLINE mkPutGroupPolicyRequest #-}

data PutGroupPolicy = PutGroupPolicy
    { _pgprGroupName :: Text
      -- ^ Name of the group to associate the policy with.
    , _pgprPolicyName :: Text
      -- ^ Name of the policy document.
    , _pgprPolicyDocument :: Text
      -- ^ The policy document.
    } deriving (Show, Generic)

-- | Name of the group to associate the policy with.
pgprGroupName :: Lens' PutGroupPolicy (Text)
pgprGroupName = lens _pgprGroupName (\s a -> s { _pgprGroupName = a })
{-# INLINE pgprGroupName #-}

-- | Name of the policy document.
pgprPolicyName :: Lens' PutGroupPolicy (Text)
pgprPolicyName = lens _pgprPolicyName (\s a -> s { _pgprPolicyName = a })
{-# INLINE pgprPolicyName #-}

-- | The policy document.
pgprPolicyDocument :: Lens' PutGroupPolicy (Text)
pgprPolicyDocument = lens _pgprPolicyDocument (\s a -> s { _pgprPolicyDocument = a })
{-# INLINE pgprPolicyDocument #-}

instance ToQuery PutGroupPolicy where
    toQuery = genericQuery def

data PutGroupPolicyResponse = PutGroupPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutGroupPolicy where
    type Sv PutGroupPolicy = IAM
    type Rs PutGroupPolicy = PutGroupPolicyResponse

    request = post "PutGroupPolicy"
    response _ = nullaryResponse PutGroupPolicyResponse
