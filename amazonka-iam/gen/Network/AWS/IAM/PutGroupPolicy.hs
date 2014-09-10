{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.PutGroupPolicy
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
module Network.AWS.IAM.PutGroupPolicy
    (
    -- * Request
      PutGroupPolicy
    -- ** Request constructor
    , mkPutGroupPolicy
    -- ** Request lenses
    , pgpGroupName
    , pgpPolicyName
    , pgpPolicyDocument

    -- * Response
    , PutGroupPolicyResponse
    -- ** Response constructor
    , mkPutGroupPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data PutGroupPolicy = PutGroupPolicy
    { _pgpGroupName :: !Text
    , _pgpPolicyName :: !Text
    , _pgpPolicyDocument :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutGroupPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @PolicyDocument ::@ @Text@
--
mkPutGroupPolicy :: Text -- ^ 'pgpGroupName'
                 -> Text -- ^ 'pgpPolicyName'
                 -> Text -- ^ 'pgpPolicyDocument'
                 -> PutGroupPolicy
mkPutGroupPolicy p1 p2 p3 = PutGroupPolicy
    { _pgpGroupName = p1
    , _pgpPolicyName = p2
    , _pgpPolicyDocument = p3
    }

-- | Name of the group to associate the policy with.
pgpGroupName :: Lens' PutGroupPolicy Text
pgpGroupName = lens _pgpGroupName (\s a -> s { _pgpGroupName = a })

-- | Name of the policy document.
pgpPolicyName :: Lens' PutGroupPolicy Text
pgpPolicyName = lens _pgpPolicyName (\s a -> s { _pgpPolicyName = a })

-- | The policy document.
pgpPolicyDocument :: Lens' PutGroupPolicy Text
pgpPolicyDocument =
    lens _pgpPolicyDocument (\s a -> s { _pgpPolicyDocument = a })

instance ToQuery PutGroupPolicy where
    toQuery = genericQuery def

data PutGroupPolicyResponse = PutGroupPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutGroupPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutGroupPolicyResponse :: PutGroupPolicyResponse
mkPutGroupPolicyResponse = PutGroupPolicyResponse

instance AWSRequest PutGroupPolicy where
    type Sv PutGroupPolicy = IAM
    type Rs PutGroupPolicy = PutGroupPolicyResponse

    request = post "PutGroupPolicy"
    response _ = nullaryResponse PutGroupPolicyResponse
