{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters the specified AMI. After you deregister an AMI, it can't be
-- used to launch new instances. This command does not delete the AMI. Example
-- This example request deregisters the specified AMI.
-- https://ec2.amazonaws.com/?Action=DeregisterImage &amp;ImageId=ami-4fa54026
-- &amp;AUTHPARAMS &lt;DeregisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeregisterImageResponse&gt;.
module Network.AWS.EC2.DeregisterImage
    (
    -- * Request
      DeregisterImage
    -- ** Request constructor
    , mkDeregisterImage
    -- ** Request lenses
    , diImageId

    -- * Response
    , DeregisterImageResponse
    -- ** Response constructor
    , mkDeregisterImageResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeregisterImage = DeregisterImage
    { _diImageId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterImage' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Text@
--
mkDeregisterImage :: Text -- ^ 'diImageId'
                  -> DeregisterImage
mkDeregisterImage p1 = DeregisterImage
    { _diImageId = p1
    }

-- | The ID of the AMI.
diImageId :: Lens' DeregisterImage Text
diImageId = lens _diImageId (\s a -> s { _diImageId = a })

instance ToQuery DeregisterImage where
    toQuery = genericQuery def

data DeregisterImageResponse = DeregisterImageResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterImageResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeregisterImageResponse :: DeregisterImageResponse
mkDeregisterImageResponse = DeregisterImageResponse

instance AWSRequest DeregisterImage where
    type Sv DeregisterImage = EC2
    type Rs DeregisterImage = DeregisterImageResponse

    request = post "DeregisterImage"
    response _ = nullaryResponse DeregisterImageResponse
