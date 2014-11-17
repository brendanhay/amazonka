{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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

-- | Deletes a DB security group.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup
    { _ddbsgDBSecurityGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteDBSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgDBSecurityGroupName' @::@ 'Text'
--
deleteDBSecurityGroup :: Text -- ^ 'ddbsgDBSecurityGroupName'
                      -> DeleteDBSecurityGroup
deleteDBSecurityGroup p1 = DeleteDBSecurityGroup
    { _ddbsgDBSecurityGroupName = p1
    }

-- | The name of the DB security group to delete. Constraints: Must be 1 to
-- 255 alphanumeric characters First character must be a letter Cannot end
-- with a hyphen or contain two consecutive hyphens Must not be "Default"
-- May not contain spaces.
ddbsgDBSecurityGroupName :: Lens' DeleteDBSecurityGroup Text
ddbsgDBSecurityGroupName =
    lens _ddbsgDBSecurityGroupName
        (\s a -> s { _ddbsgDBSecurityGroupName = a })

data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDBSecurityGroupResponse' constructor.
deleteDBSecurityGroupResponse :: DeleteDBSecurityGroupResponse
deleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse

instance AWSRequest DeleteDBSecurityGroup where
    type Sv DeleteDBSecurityGroup = RDS
    type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse

    request  = post "DeleteDBSecurityGroup"
    response = nullResponse DeleteDBSecurityGroupResponse

instance ToPath DeleteDBSecurityGroup where
    toPath = const "/"

instance ToHeaders DeleteDBSecurityGroup

instance ToQuery DeleteDBSecurityGroup
