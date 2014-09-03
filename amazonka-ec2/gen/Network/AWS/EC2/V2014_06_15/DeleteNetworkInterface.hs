{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified network interface. You must detach the network
-- interface before you can delete it. Example This example deletes the
-- specified network interface.
-- https://ec2.amazonaws.com/?Action=DeleteNetworkInterface
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;AUTHPARAMS
-- &lt;DeleteNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;e1c6d73b-edaa-4e62-9909-6611404e1739&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteNetworkInterface where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteNetworkInterface' request.
deleteNetworkInterface :: Text -- ^ '_dnirNetworkInterfaceId'
                       -> DeleteNetworkInterface
deleteNetworkInterface p1 = DeleteNetworkInterface
    { _dnirNetworkInterfaceId = p1
    }

data DeleteNetworkInterface = DeleteNetworkInterface
    { _dnirNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    } deriving (Show, Generic)

makeLenses ''DeleteNetworkInterface

instance ToQuery DeleteNetworkInterface where
    toQuery = genericQuery def

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteNetworkInterfaceResponse

instance AWSRequest DeleteNetworkInterface where
    type Sv DeleteNetworkInterface = EC2
    type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse

    request = post "DeleteNetworkInterface"
    response _ = nullaryResponse DeleteNetworkInterfaceResponse
