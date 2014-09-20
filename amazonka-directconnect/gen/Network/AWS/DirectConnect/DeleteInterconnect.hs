{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified interconnect.
module Network.AWS.DirectConnect.DeleteInterconnect
    (
    -- * Request
      DeleteInterconnect
    -- ** Request constructor
    , deleteInterconnect
    -- ** Request lenses
    , diInterconnectId

    -- * Response
    , DeleteInterconnectResponse
    -- ** Response constructor
    , deleteInterconnectResponse
    -- ** Response lenses
    , dirInterconnectState
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the DeleteInterconnect operation.
newtype DeleteInterconnect = DeleteInterconnect
    { _diInterconnectId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInterconnect' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectId ::@ @Text@
--
deleteInterconnect :: Text -- ^ 'diInterconnectId'
                   -> DeleteInterconnect
deleteInterconnect p1 = DeleteInterconnect
    { _diInterconnectId = p1
    }

-- | The ID of the interconnect. Example: dxcon-abc123.
diInterconnectId :: Lens' DeleteInterconnect Text
diInterconnectId =
    lens _diInterconnectId (\s a -> s { _diInterconnectId = a })

instance ToPath DeleteInterconnect

instance ToQuery DeleteInterconnect

instance ToHeaders DeleteInterconnect

instance ToJSON DeleteInterconnect

-- | The response received when DeleteInterconnect is called.
newtype DeleteInterconnectResponse = DeleteInterconnectResponse
    { _dirInterconnectState :: Maybe InterconnectState
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInterconnectResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectState ::@ @Maybe InterconnectState@
--
deleteInterconnectResponse :: DeleteInterconnectResponse
deleteInterconnectResponse = DeleteInterconnectResponse
    { _dirInterconnectState = Nothing
    }

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
dirInterconnectState :: Lens' DeleteInterconnectResponse (Maybe InterconnectState)
dirInterconnectState =
    lens _dirInterconnectState (\s a -> s { _dirInterconnectState = a })

instance FromJSON DeleteInterconnectResponse

instance AWSRequest DeleteInterconnect where
    type Sv DeleteInterconnect = DirectConnect
    type Rs DeleteInterconnect = DeleteInterconnectResponse

    request = get
    response _ = jsonResponse
