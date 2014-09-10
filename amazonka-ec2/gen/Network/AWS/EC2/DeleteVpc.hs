{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and so
-- on. Example This example deletes the specified VPC.
-- https://ec2.amazonaws.com/?Action=DeleteVpc &amp;VpcId=vpc-1a2b3c4d
-- &amp;AUTHPARAMS &lt;DeleteVpcResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpcResponse&gt;.
module Network.AWS.EC2.DeleteVpc
    (
    -- * Request
      DeleteVpc
    -- ** Request constructor
    , mkDeleteVpc
    -- ** Request lenses
    , dv1VpcId

    -- * Response
    , DeleteVpcResponse
    -- ** Response constructor
    , mkDeleteVpcResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteVpc = DeleteVpc
    { _dv1VpcId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpc' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Text@
--
mkDeleteVpc :: Text -- ^ 'dv1VpcId'
            -> DeleteVpc
mkDeleteVpc p1 = DeleteVpc
    { _dv1VpcId = p1
    }

-- | The ID of the VPC.
dv1VpcId :: Lens' DeleteVpc Text
dv1VpcId = lens _dv1VpcId (\s a -> s { _dv1VpcId = a })

instance ToQuery DeleteVpc where
    toQuery = genericQuery def

data DeleteVpcResponse = DeleteVpcResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpcResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteVpcResponse :: DeleteVpcResponse
mkDeleteVpcResponse = DeleteVpcResponse

instance AWSRequest DeleteVpc where
    type Sv DeleteVpc = EC2
    type Rs DeleteVpc = DeleteVpcResponse

    request = post "DeleteVpc"
    response _ = nullaryResponse DeleteVpcResponse
