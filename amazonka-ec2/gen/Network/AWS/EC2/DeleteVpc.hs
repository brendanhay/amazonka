{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpc
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete all
-- route tables associated with the VPC (except the default one), and so on.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpc.html>
module Network.AWS.EC2.DeleteVpc
    (
    -- * Request
      DeleteVpc
    -- ** Request constructor
    , deleteVpc
    -- ** Request lenses
    , dv3DryRun
    , dv3VpcId

    -- * Response
    , DeleteVpcResponse
    -- ** Response constructor
    , deleteVpcResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVpc = DeleteVpc
    { _dv3DryRun :: Maybe Bool
    , _dv3VpcId  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv3DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv3VpcId' @::@ 'Text'
--
deleteVpc :: Text -- ^ 'dv3VpcId'
          -> DeleteVpc
deleteVpc p1 = DeleteVpc
    { _dv3VpcId  = p1
    , _dv3DryRun = Nothing
    }

dv3DryRun :: Lens' DeleteVpc (Maybe Bool)
dv3DryRun = lens _dv3DryRun (\s a -> s { _dv3DryRun = a })

-- | The ID of the VPC.
dv3VpcId :: Lens' DeleteVpc Text
dv3VpcId = lens _dv3VpcId (\s a -> s { _dv3VpcId = a })

data DeleteVpcResponse = DeleteVpcResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteVpcResponse' constructor.
deleteVpcResponse :: DeleteVpcResponse
deleteVpcResponse = DeleteVpcResponse

instance ToPath DeleteVpc where
    toPath = const "/"

instance ToQuery DeleteVpc where
    toQuery DeleteVpc{..} = mconcat
        [ "DryRun" =? _dv3DryRun
        , "VpcId"  =? _dv3VpcId
        ]

instance ToHeaders DeleteVpc

instance AWSRequest DeleteVpc where
    type Sv DeleteVpc = EC2
    type Rs DeleteVpc = DeleteVpcResponse

    request  = post "DeleteVpc"
    response = nullResponse DeleteVpcResponse
