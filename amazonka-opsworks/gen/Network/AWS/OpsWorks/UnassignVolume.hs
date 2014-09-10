{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Unassigns an assigned Amazon EBS volume. The volume remains registered with
-- the stack. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks
    (
    -- * Request
      UnassignVolume
    -- ** Request constructor
    , mkUnassignVolume
    -- ** Request lenses
    , uvVolumeId

    -- * Response
    , UnassignVolumeResponse
    -- ** Response constructor
    , mkUnassignVolumeResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype UnassignVolume = UnassignVolume
    { _uvVolumeId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnassignVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
mkUnassignVolume :: Text -- ^ 'uvVolumeId'
                 -> UnassignVolume
mkUnassignVolume p1 = UnassignVolume
    { _uvVolumeId = p1
    }

-- | The volume ID.
uvVolumeId :: Lens' UnassignVolume Text
uvVolumeId = lens _uvVolumeId (\s a -> s { _uvVolumeId = a })

instance ToPath UnassignVolume

instance ToQuery UnassignVolume

instance ToHeaders UnassignVolume

instance ToJSON UnassignVolume

data UnassignVolumeResponse = UnassignVolumeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnassignVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUnassignVolumeResponse :: UnassignVolumeResponse
mkUnassignVolumeResponse = UnassignVolumeResponse

instance AWSRequest UnassignVolume where
    type Sv UnassignVolume = OpsWorks
    type Rs UnassignVolume = UnassignVolumeResponse

    request = get
    response _ = nullaryResponse UnassignVolumeResponse
