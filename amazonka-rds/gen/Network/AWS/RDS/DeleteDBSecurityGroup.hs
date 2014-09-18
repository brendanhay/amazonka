{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
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
module Network.AWS.RDS.DeleteDBSecurityGroup
    (
    -- * Request
      DeleteDBSecurityGroup
    -- ** Request constructor
    , deleteDBSecurityGroup
    -- ** Request lenses
    , ddbsgDBSecurityGroupName

    -- * Response
    , DeleteDBSecurityGroupResponse
    -- ** Response constructor
    , deleteDBSecurityGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup
    { _ddbsgDBSecurityGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDBSecurityGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroupName ::@ @Text@
--
deleteDBSecurityGroup :: Text -- ^ 'ddbsgDBSecurityGroupName'
                        -> DeleteDBSecurityGroup
deleteDBSecurityGroup p1 = DeleteDBSecurityGroup
    { _ddbsgDBSecurityGroupName = p1
    }

-- | The name of the DB security group to delete. You cannot delete the default
-- DB security group. Constraints: Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Must not be "Default" May not contain spaces.
ddbsgDBSecurityGroupName :: Lens' DeleteDBSecurityGroup Text
ddbsgDBSecurityGroupName =
    lens _ddbsgDBSecurityGroupName
         (\s a -> s { _ddbsgDBSecurityGroupName = a })

instance ToQuery DeleteDBSecurityGroup where
    toQuery = genericQuery def

data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDBSecurityGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteDBSecurityGroupResponse :: DeleteDBSecurityGroupResponse
deleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse

instance AWSRequest DeleteDBSecurityGroup where
    type Sv DeleteDBSecurityGroup = RDS
    type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse

    request = post "DeleteDBSecurityGroup"
    response _ = nullaryResponse DeleteDBSecurityGroupResponse
