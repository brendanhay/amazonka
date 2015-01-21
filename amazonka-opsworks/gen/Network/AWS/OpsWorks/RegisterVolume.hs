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

-- Module      : Network.AWS.OpsWorks.RegisterVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Registers an Amazon EBS volume with a specified stack. A volume can be
-- registered with only one stack at a time. If the volume is already
-- registered, you must first deregister it by calling 'DeregisterVolume'. For
-- more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterVolume.html>
module Network.AWS.OpsWorks.RegisterVolume
    (
    -- * Request
      RegisterVolume
    -- ** Request constructor
    , registerVolume
    -- ** Request lenses
    , rvEc2VolumeId
    , rvStackId

    -- * Response
    , RegisterVolumeResponse
    -- ** Response constructor
    , registerVolumeResponse
    -- ** Response lenses
    , rvrVolumeId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data RegisterVolume = RegisterVolume
    { _rvEc2VolumeId :: Maybe Text
    , _rvStackId     :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RegisterVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvEc2VolumeId' @::@ 'Maybe' 'Text'
--
-- * 'rvStackId' @::@ 'Text'
--
registerVolume :: Text -- ^ 'rvStackId'
               -> RegisterVolume
registerVolume p1 = RegisterVolume
    { _rvStackId     = p1
    , _rvEc2VolumeId = Nothing
    }

-- | The Amazon EBS volume ID.
rvEc2VolumeId :: Lens' RegisterVolume (Maybe Text)
rvEc2VolumeId = lens _rvEc2VolumeId (\s a -> s { _rvEc2VolumeId = a })

-- | The stack ID.
rvStackId :: Lens' RegisterVolume Text
rvStackId = lens _rvStackId (\s a -> s { _rvStackId = a })

newtype RegisterVolumeResponse = RegisterVolumeResponse
    { _rvrVolumeId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'RegisterVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvrVolumeId' @::@ 'Maybe' 'Text'
--
registerVolumeResponse :: RegisterVolumeResponse
registerVolumeResponse = RegisterVolumeResponse
    { _rvrVolumeId = Nothing
    }

-- | The volume ID.
rvrVolumeId :: Lens' RegisterVolumeResponse (Maybe Text)
rvrVolumeId = lens _rvrVolumeId (\s a -> s { _rvrVolumeId = a })

instance ToPath RegisterVolume where
    toPath = const "/"

instance ToQuery RegisterVolume where
    toQuery = const mempty

instance ToHeaders RegisterVolume

instance ToJSON RegisterVolume where
    toJSON RegisterVolume{..} = object
        [ "Ec2VolumeId" .= _rvEc2VolumeId
        , "StackId"     .= _rvStackId
        ]

instance AWSRequest RegisterVolume where
    type Sv RegisterVolume = OpsWorks
    type Rs RegisterVolume = RegisterVolumeResponse

    request  = post "RegisterVolume"
    response = jsonResponse

instance FromJSON RegisterVolumeResponse where
    parseJSON = withObject "RegisterVolumeResponse" $ \o -> RegisterVolumeResponse
        <$> o .:? "VolumeId"
