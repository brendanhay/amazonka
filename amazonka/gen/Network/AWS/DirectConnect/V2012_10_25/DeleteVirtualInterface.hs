{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DeleteVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a virtual interface.
module Network.AWS.DirectConnect.V2012_10_25.DeleteVirtualInterface where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude

data DeleteVirtualInterface = DeleteVirtualInterface
    { _dvisVirtualInterfaceId :: Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    } deriving (Show, Generic)

makeLenses ''DeleteVirtualInterface

instance ToPath DeleteVirtualInterface

instance ToQuery DeleteVirtualInterface

instance ToHeaders DeleteVirtualInterface

instance ToJSON DeleteVirtualInterface

data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse
    { _dvitVirtualInterfaceState :: Maybe VirtualInterfaceState
      -- ^ State of the virtual interface. Confirming: The creation of the
      -- virtual interface is pending confirmation from the virtual
      -- interface owner. If the owner of the virtual interface is
      -- different from the owner of the connection on which it is
      -- provisioned, then the virtual interface will remain in this state
      -- until it is confirmed by the virtual interface owner. Verifying:
      -- This state only applies to public virtual interfaces. Each public
      -- virtual interface needs validation before the virtual interface
      -- can be created. Pending: A virtual interface is in this state
      -- from the time that it is created until the virtual interface is
      -- ready to forward traffic. Available: A virtual interface that is
      -- able to forward traffic. Deleting: A virtual interface is in this
      -- state immediately after calling DeleteVirtualInterface until it
      -- can no longer forward traffic. Deleted: A virtual interface that
      -- cannot forward traffic. Rejected: The virtual interface owner has
      -- declined creation of the virtual interface. If a virtual
      -- interface in the 'Confirming' state is deleted by the virtual
      -- interface owner, the virtual interface will enter the 'Rejected'
      -- state.
    } deriving (Show, Generic)

makeLenses ''DeleteVirtualInterfaceResponse

instance FromJSON DeleteVirtualInterfaceResponse

instance AWSRequest DeleteVirtualInterface where
    type Sv DeleteVirtualInterface = DirectConnect
    type Rs DeleteVirtualInterface = DeleteVirtualInterfaceResponse

    request = get
    response _ = undefined
