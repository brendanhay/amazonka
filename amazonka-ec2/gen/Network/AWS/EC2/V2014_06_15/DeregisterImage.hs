{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeregisterImage
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
module Network.AWS.EC2.V2014_06_15.DeregisterImage where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeregisterImage' request.
deregisterImage :: Text -- ^ '_dirImageId'
                -> DeregisterImage
deregisterImage p1 = DeregisterImage
    { _dirImageId = p1
    }

data DeregisterImage = DeregisterImage
    { _dirImageId :: Text
      -- ^ The ID of the AMI.
    } deriving (Show, Generic)

makeLenses ''DeregisterImage

instance ToQuery DeregisterImage where
    toQuery = genericQuery def

data DeregisterImageResponse = DeregisterImageResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeregisterImageResponse

instance AWSRequest DeregisterImage where
    type Sv DeregisterImage = EC2
    type Rs DeregisterImage = DeregisterImageResponse

    request = post "DeregisterImage"
    response _ = nullaryResponse DeregisterImageResponse
