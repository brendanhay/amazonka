{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified subnet. You must terminate all running instances in
-- the subnet before you can delete the subnet. Example This example deletes
-- the specified subnet. https://ec2.amazonaws.com/?Action=DeleteSubnet
-- &amp;SubnetId=subnet-9d4a7b6c &amp;AUTHPARAMS &lt;DeleteSubnetResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSubnetResponse&gt;.
module Network.AWS.EC2.DeleteSubnet
    (
    -- * Request
      DeleteSubnet
    -- ** Request constructor
    , deleteSubnet
    -- ** Request lenses
    , ds1SubnetId

    -- * Response
    , DeleteSubnetResponse
    -- ** Response constructor
    , deleteSubnetResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteSubnet = DeleteSubnet
    { _ds1SubnetId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSubnet' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetId ::@ @Text@
--
deleteSubnet :: Text -- ^ 'ds1SubnetId'
             -> DeleteSubnet
deleteSubnet p1 = DeleteSubnet
    { _ds1SubnetId = p1
    }

-- | The ID of the subnet.
ds1SubnetId :: Lens' DeleteSubnet Text
ds1SubnetId = lens _ds1SubnetId (\s a -> s { _ds1SubnetId = a })

instance ToQuery DeleteSubnet where
    toQuery = genericQuery def

data DeleteSubnetResponse = DeleteSubnetResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSubnetResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteSubnetResponse :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse

instance AWSRequest DeleteSubnet where
    type Sv DeleteSubnet = EC2
    type Rs DeleteSubnet = DeleteSubnetResponse

    request = post "DeleteSubnet"
    response _ = nullaryResponse DeleteSubnetResponse
