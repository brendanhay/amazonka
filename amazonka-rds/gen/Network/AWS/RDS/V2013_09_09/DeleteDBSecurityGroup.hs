{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DB security group. The specified DB security group must not be
-- associated with any DB instances. https://rds.amazonaws.com/
-- ?Action=DeleteDBSecurityGroup &DBSecurityGroupName=mysecuritygroup
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T17%3A48%3A21.746Z &AWSAccessKeyId= &Signature=
-- 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.RDS.V2013_09_09.DeleteDBSecurityGroup
    (
    -- * Request
      DeleteDBSecurityGroup
    -- ** Request constructor
    , mkDeleteDBSecurityGroupMessage
    -- ** Request lenses
    , ddbsgmDBSecurityGroupName

    -- * Response
    , DeleteDBSecurityGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDBSecurityGroup' request.
mkDeleteDBSecurityGroupMessage :: Text -- ^ 'ddbsgmDBSecurityGroupName'
                               -> DeleteDBSecurityGroup
mkDeleteDBSecurityGroupMessage p1 = DeleteDBSecurityGroup
    { _ddbsgmDBSecurityGroupName = p1
    }
{-# INLINE mkDeleteDBSecurityGroupMessage #-}

newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup
    { _ddbsgmDBSecurityGroupName :: Text
      -- ^ The name of the DB security group to delete. You cannot delete
      -- the default DB security group. Constraints: Must be 1 to 255
      -- alphanumeric characters First character must be a letter Cannot
      -- end with a hyphen or contain two consecutive hyphens Must not be
      -- "Default" May not contain spaces.
    } deriving (Show, Generic)

-- | The name of the DB security group to delete. You cannot delete the default
-- DB security group. Constraints: Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Must not be "Default" May not contain spaces.
ddbsgmDBSecurityGroupName :: Lens' DeleteDBSecurityGroup (Text)
ddbsgmDBSecurityGroupName = lens _ddbsgmDBSecurityGroupName (\s a -> s { _ddbsgmDBSecurityGroupName = a })
{-# INLINE ddbsgmDBSecurityGroupName #-}

instance ToQuery DeleteDBSecurityGroup where
    toQuery = genericQuery def

data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteDBSecurityGroup where
    type Sv DeleteDBSecurityGroup = RDS
    type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse

    request = post "DeleteDBSecurityGroup"
    response _ = nullaryResponse DeleteDBSecurityGroupResponse
