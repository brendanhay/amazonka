{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
module Network.AWS.OpsWorks.UnassignVolume
    (
    -- * Request
      UnassignVolume
    -- ** Request constructor
    , unassignVolume
    -- ** Request lenses
    , uvVolumeId

    -- * Response
    , UnassignVolumeResponse
    -- ** Response constructor
    , unassignVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype UnassignVolume = UnassignVolume
    { _uvVolumeId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'UnassignVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvVolumeId' @::@ 'Text'
--
unassignVolume :: Text -- ^ 'uvVolumeId'
               -> UnassignVolume
unassignVolume p1 = UnassignVolume
    { _uvVolumeId = p1
    }

-- | The volume ID.
uvVolumeId :: Lens' UnassignVolume Text
uvVolumeId = lens _uvVolumeId (\s a -> s { _uvVolumeId = a })

instance ToPath UnassignVolume where
    toPath = const "/"

instance ToQuery UnassignVolume where
    toQuery = const mempty

instance ToHeaders UnassignVolume

instance ToBody UnassignVolume where
    toBody = toBody . encode . _uvVolumeId

data UnassignVolumeResponse = UnassignVolumeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UnassignVolumeResponse' constructor.
unassignVolumeResponse :: UnassignVolumeResponse
unassignVolumeResponse = UnassignVolumeResponse

instance AWSRequest UnassignVolume where
    type Sv UnassignVolume = OpsWorks
    type Rs UnassignVolume = UnassignVolumeResponse

    request  = post
    response = nullaryResponse UnassignVolumeResponse
