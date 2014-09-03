{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DeleteInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified interconnect.
module Network.AWS.DirectConnect.V2012_10_25.DeleteInterconnect
    (
    -- * Request
      DeleteInterconnect
    -- ** Request constructor
    , deleteInterconnect
    -- ** Request lenses
    , dirInterconnectId

    -- * Response
    , DeleteInterconnectResponse
    -- ** Response lenses
    , disInterconnectState
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteInterconnect' request.
deleteInterconnect :: Text -- ^ 'dirInterconnectId'
                   -> DeleteInterconnect
deleteInterconnect p1 = DeleteInterconnect
    { _dirInterconnectId = p1
    }

data DeleteInterconnect = DeleteInterconnect
    { _dirInterconnectId :: Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    } deriving (Show, Generic)

-- | The ID of the interconnect. Example: dxcon-abc123.
dirInterconnectId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteInterconnect
    -> f DeleteInterconnect
dirInterconnectId f x =
    (\y -> x { _dirInterconnectId = y })
       <$> f (_dirInterconnectId x)
{-# INLINE dirInterconnectId #-}

instance ToPath DeleteInterconnect

instance ToQuery DeleteInterconnect

instance ToHeaders DeleteInterconnect

instance ToJSON DeleteInterconnect

data DeleteInterconnectResponse = DeleteInterconnectResponse
    { _disInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an
      -- interconnect. The interconnect stays in the requested state until
      -- the Letter of Authorization (LOA) is sent to the customer.
      -- Pending: The interconnect has been approved, and is being
      -- initialized. Available: The network link is up, and the
      -- interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    } deriving (Show, Generic)

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
disInterconnectState
    :: Functor f
    => (Maybe InterconnectState
    -> f (Maybe InterconnectState))
    -> DeleteInterconnectResponse
    -> f DeleteInterconnectResponse
disInterconnectState f x =
    (\y -> x { _disInterconnectState = y })
       <$> f (_disInterconnectState x)
{-# INLINE disInterconnectState #-}

instance FromJSON DeleteInterconnectResponse

instance AWSRequest DeleteInterconnect where
    type Sv DeleteInterconnect = DirectConnect
    type Rs DeleteInterconnect = DeleteInterconnectResponse

    request = get
    response _ = jsonResponse
