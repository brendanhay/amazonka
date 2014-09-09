{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy that is associated with the specified group.
-- https://iam.amazonaws.com/ ?Action=DeleteGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy
    (
    -- * Request
      DeleteGroupPolicy
    -- ** Request constructor
    , mkDeleteGroupPolicy
    -- ** Request lenses
    , dgpGroupName
    , dgpPolicyName

    -- * Response
    , DeleteGroupPolicyResponse
    -- ** Response constructor
    , mkDeleteGroupPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeleteGroupPolicy = DeleteGroupPolicy
    { _dgpGroupName :: Text
    , _dgpPolicyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGroupPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
mkDeleteGroupPolicy :: Text -- ^ 'dgpGroupName'
                    -> Text -- ^ 'dgpPolicyName'
                    -> DeleteGroupPolicy
mkDeleteGroupPolicy p1 p2 = DeleteGroupPolicy
    { _dgpGroupName = p1
    , _dgpPolicyName = p2
    }

-- | Name of the group the policy is associated with.
dgpGroupName :: Lens' DeleteGroupPolicy Text
dgpGroupName = lens _dgpGroupName (\s a -> s { _dgpGroupName = a })

-- | Name of the policy document to delete.
dgpPolicyName :: Lens' DeleteGroupPolicy Text
dgpPolicyName = lens _dgpPolicyName (\s a -> s { _dgpPolicyName = a })

instance ToQuery DeleteGroupPolicy where
    toQuery = genericQuery def

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGroupPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteGroupPolicyResponse :: DeleteGroupPolicyResponse
mkDeleteGroupPolicyResponse = DeleteGroupPolicyResponse

instance AWSRequest DeleteGroupPolicy where
    type Sv DeleteGroupPolicy = IAM
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse

    request = post "DeleteGroupPolicy"
    response _ = nullaryResponse DeleteGroupPolicyResponse
