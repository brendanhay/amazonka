{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified role. For
-- information about policies, go to Overview of Policies in the Using IAM
-- guide. For information about limits on the policies you can associate with
-- a role, see Limitations on IAM Entities in the Using IAM guide. Because
-- policy documents can be large, you should use POST rather than GET when
-- calling PutRolePolicy. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutRolePolicy &RoleName=S3Access
-- &PolicyName=S3AccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"s3:*","Resource":"*"}]}
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Request
      PutRolePolicy
    -- ** Request constructor
    , mkPutRolePolicy
    -- ** Request lenses
    , prpRoleName
    , prpPolicyName
    , prpPolicyDocument

    -- * Response
    , PutRolePolicyResponse
    -- ** Response constructor
    , mkPutRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data PutRolePolicy = PutRolePolicy
    { _prpRoleName :: !Text
    , _prpPolicyName :: !Text
    , _prpPolicyDocument :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutRolePolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @PolicyDocument ::@ @Text@
--
mkPutRolePolicy :: Text -- ^ 'prpRoleName'
                -> Text -- ^ 'prpPolicyName'
                -> Text -- ^ 'prpPolicyDocument'
                -> PutRolePolicy
mkPutRolePolicy p1 p2 p3 = PutRolePolicy
    { _prpRoleName = p1
    , _prpPolicyName = p2
    , _prpPolicyDocument = p3
    }

-- | Name of the role to associate the policy with.
prpRoleName :: Lens' PutRolePolicy Text
prpRoleName = lens _prpRoleName (\s a -> s { _prpRoleName = a })

-- | Name of the policy document.
prpPolicyName :: Lens' PutRolePolicy Text
prpPolicyName = lens _prpPolicyName (\s a -> s { _prpPolicyName = a })

-- | The policy document.
prpPolicyDocument :: Lens' PutRolePolicy Text
prpPolicyDocument =
    lens _prpPolicyDocument (\s a -> s { _prpPolicyDocument = a })

instance ToQuery PutRolePolicy where
    toQuery = genericQuery def

data PutRolePolicyResponse = PutRolePolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutRolePolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutRolePolicyResponse :: PutRolePolicyResponse
mkPutRolePolicyResponse = PutRolePolicyResponse

instance AWSRequest PutRolePolicy where
    type Sv PutRolePolicy = IAM
    type Rs PutRolePolicy = PutRolePolicyResponse

    request = post "PutRolePolicy"
    response _ = nullaryResponse PutRolePolicyResponse
