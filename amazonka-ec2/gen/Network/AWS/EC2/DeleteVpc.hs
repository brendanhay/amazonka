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

-- Module      : Network.AWS.EC2.DeleteVpc
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and so
-- on.
module Network.AWS.EC2.DeleteVpc
    (
    -- * Request
      DeleteVpc
    -- ** Request constructor
    , deleteVpc
    -- ** Request lenses
    , dv2DryRun
    , dv2VpcId

    -- * Response
    , DeleteVpcResponse
    -- ** Response constructor
    , deleteVpcResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteVpc = DeleteVpc
    { _dv2DryRun :: Maybe Bool
    , _dv2VpcId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv2VpcId' @::@ 'Text'
--
deleteVpc :: Text -- ^ 'dv2VpcId'
          -> DeleteVpc
deleteVpc p1 = DeleteVpc
    { _dv2VpcId  = p1
    , _dv2DryRun = Nothing
    }

dv2DryRun :: Lens' DeleteVpc (Maybe Bool)
dv2DryRun = lens _dv2DryRun (\s a -> s { _dv2DryRun = a })

-- | The ID of the VPC.
dv2VpcId :: Lens' DeleteVpc Text
dv2VpcId = lens _dv2VpcId (\s a -> s { _dv2VpcId = a })

instance ToPath DeleteVpc where
    toPath = const "/"

instance ToQuery DeleteVpc

data DeleteVpcResponse = DeleteVpcResponse

-- | 'DeleteVpcResponse' constructor.
deleteVpcResponse :: DeleteVpcResponse
deleteVpcResponse = DeleteVpcResponse

instance AWSRequest DeleteVpc where
    type Sv DeleteVpc = EC2
    type Rs DeleteVpc = DeleteVpcResponse

    request  = post "DeleteVpc"
    response = const (nullaryResponse DeleteVpcResponse)
