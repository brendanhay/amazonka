{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an Amazon Redshift security group. For information about managing
-- security groups, go to Amazon Redshift Cluster Security Groups in the
-- Amazon Redshift Management Guide.
module Network.AWS.Redshift.DeleteClusterSecurityGroup
    (
    -- * Request
      DeleteClusterSecurityGroup
    -- ** Request constructor
    , deleteClusterSecurityGroup
    -- ** Request lenses
    , dcsg1ClusterSecurityGroupName

    -- * Response
    , DeleteClusterSecurityGroupResponse
    -- ** Response constructor
    , deleteClusterSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype DeleteClusterSecurityGroup = DeleteClusterSecurityGroup
    { _dcsg1ClusterSecurityGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteClusterSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsg1ClusterSecurityGroupName' @::@ 'Text'
--
deleteClusterSecurityGroup :: Text -- ^ 'dcsg1ClusterSecurityGroupName'
                           -> DeleteClusterSecurityGroup
deleteClusterSecurityGroup p1 = DeleteClusterSecurityGroup
    { _dcsg1ClusterSecurityGroupName = p1
    }

-- | The name of the cluster security group to be deleted.
dcsg1ClusterSecurityGroupName :: Lens' DeleteClusterSecurityGroup Text
dcsg1ClusterSecurityGroupName =
    lens _dcsg1ClusterSecurityGroupName
        (\s a -> s { _dcsg1ClusterSecurityGroupName = a })

instance ToQuery DeleteClusterSecurityGroup

instance ToPath DeleteClusterSecurityGroup where
    toPath = const "/"

data DeleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteClusterSecurityGroupResponse' constructor.
deleteClusterSecurityGroupResponse :: DeleteClusterSecurityGroupResponse
deleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse

instance AWSRequest DeleteClusterSecurityGroup where
    type Sv DeleteClusterSecurityGroup = Redshift
    type Rs DeleteClusterSecurityGroup = DeleteClusterSecurityGroupResponse

    request  = post "DeleteClusterSecurityGroup"
    response = nullaryResponse DeleteClusterSecurityGroupResponse
