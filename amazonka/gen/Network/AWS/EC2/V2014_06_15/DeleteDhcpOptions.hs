{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified set of DHCP options. You must disassociate the set of
-- DHCP options before you can delete it. You can disassociate the set of DHCP
-- options by associating either a new set of options or the default set of
-- options with the VPC. Example This example deletes the specified set of
-- DHCP options. https://ec2.amazonaws.com/?Action=DeleteDhcpOptions
-- &amp;DhcpOptionsId=dopt-7a8b9c2d &amp;AUTHPARAMS
-- &lt;DeleteDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteDhcpOptionsResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteDhcpOptions where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDhcpOptions' request.
deleteDhcpOptions :: Text -- ^ '_ddotDhcpOptionsId'
                  -> DeleteDhcpOptions
deleteDhcpOptions p1 = DeleteDhcpOptions
    { _ddotDhcpOptionsId = p1
    , _ddotDryRun = Nothing
    }

data DeleteDhcpOptions = DeleteDhcpOptions
    { _ddotDhcpOptionsId :: Text
      -- ^ The ID of the DHCP options set.
    , _ddotDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''DeleteDhcpOptions

instance ToQuery DeleteDhcpOptions where
    toQuery = genericToQuery def

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteDhcpOptionsResponse

instance AWSRequest DeleteDhcpOptions where
    type Sv DeleteDhcpOptions = EC2
    type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse

    request = post "DeleteDhcpOptions"
    response _ _ = return (Right DeleteDhcpOptionsResponse)
