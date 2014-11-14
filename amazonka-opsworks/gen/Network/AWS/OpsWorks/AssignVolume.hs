{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Assigns one of the stack's registered Amazon EBS volumes to a specified
-- instance. The volume must first be registered with the stack by calling
-- RegisterVolume. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.AssignVolume
    (
    -- * Request
      AssignVolume
    -- ** Request constructor
    , assignVolume
    -- ** Request lenses
    , avInstanceId
    , avVolumeId

    -- * Response
    , AssignVolumeResponse
    -- ** Response constructor
    , assignVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data AssignVolume = AssignVolume
    { _avInstanceId :: Maybe Text
    , _avVolumeId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssignVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'avVolumeId' @::@ 'Text'
--
assignVolume :: Text -- ^ 'avVolumeId'
             -> AssignVolume
assignVolume p1 = AssignVolume
    { _avVolumeId   = p1
    , _avInstanceId = Nothing
    }

-- | The instance ID.
avInstanceId :: Lens' AssignVolume (Maybe Text)
avInstanceId = lens _avInstanceId (\s a -> s { _avInstanceId = a })

-- | The volume ID.
avVolumeId :: Lens' AssignVolume Text
avVolumeId = lens _avVolumeId (\s a -> s { _avVolumeId = a })

instance ToPath AssignVolume where
    toPath = const "/"

instance ToQuery AssignVolume where
    toQuery = const mempty

instance ToHeaders AssignVolume

instance ToBody AssignVolume where
    toBody = toBody . encode . _avVolumeId

data AssignVolumeResponse = AssignVolumeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AssignVolumeResponse' constructor.
assignVolumeResponse :: AssignVolumeResponse
assignVolumeResponse = AssignVolumeResponse

instance AWSRequest AssignVolume where
    type Sv AssignVolume = OpsWorks
    type Rs AssignVolume = AssignVolumeResponse

    request  = post
    response = nullaryResponse AssignVolumeResponse
