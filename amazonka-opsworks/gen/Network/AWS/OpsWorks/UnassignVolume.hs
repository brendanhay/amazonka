{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- the stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UnassignVolume.html>
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
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype UnassignVolume = UnassignVolume
    { _uvVolumeId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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
--
uvVolumeId :: Lens' UnassignVolume Text
uvVolumeId = lens _uvVolumeId (\s a -> s { _uvVolumeId = a })

data UnassignVolumeResponse = UnassignVolumeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UnassignVolumeResponse' constructor.
unassignVolumeResponse :: UnassignVolumeResponse
unassignVolumeResponse = UnassignVolumeResponse

instance ToPath UnassignVolume where
    toPath = const "/"

instance ToQuery UnassignVolume where
    toQuery = const mempty

instance ToHeaders UnassignVolume

instance ToJSON UnassignVolume where
    toJSON UnassignVolume{..} = object
        [ "VolumeId" .= _uvVolumeId
        ]

instance AWSRequest UnassignVolume where
    type Sv UnassignVolume = OpsWorks
    type Rs UnassignVolume = UnassignVolumeResponse

    request  = post "UnassignVolume"
    response = nullResponse UnassignVolumeResponse
