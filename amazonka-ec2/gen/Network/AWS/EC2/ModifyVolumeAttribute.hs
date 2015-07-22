{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVolumeAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies a volume attribute.
--
-- By default, all I\/O operations for the volume are suspended when the
-- data on the volume is determined to be potentially inconsistent, to
-- prevent undetectable, latent data corruption. The I\/O access to the
-- volume can be resumed by first enabling I\/O access and then checking
-- the data consistency on your volume.
--
-- You can change the default behavior to resume I\/O operations. We
-- recommend that you change this only for boot volumes or for volumes that
-- are stateless or disposable.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVolumeAttribute.html>
module Network.AWS.EC2.ModifyVolumeAttribute
    (
    -- * Request
      ModifyVolumeAttribute
    -- ** Request constructor
    , modifyVolumeAttribute
    -- ** Request lenses
    , mvarqAutoEnableIO
    , mvarqDryRun
    , mvarqVolumeId

    -- * Response
    , ModifyVolumeAttributeResponse
    -- ** Response constructor
    , modifyVolumeAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyVolumeAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mvarqAutoEnableIO'
--
-- * 'mvarqDryRun'
--
-- * 'mvarqVolumeId'
data ModifyVolumeAttribute = ModifyVolumeAttribute'
    { _mvarqAutoEnableIO :: !(Maybe AttributeBooleanValue)
    , _mvarqDryRun       :: !(Maybe Bool)
    , _mvarqVolumeId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVolumeAttribute' smart constructor.
modifyVolumeAttribute :: Text -> ModifyVolumeAttribute
modifyVolumeAttribute pVolumeId_ =
    ModifyVolumeAttribute'
    { _mvarqAutoEnableIO = Nothing
    , _mvarqDryRun = Nothing
    , _mvarqVolumeId = pVolumeId_
    }

-- | Indicates whether the volume should be auto-enabled for I\/O operations.
mvarqAutoEnableIO :: Lens' ModifyVolumeAttribute (Maybe AttributeBooleanValue)
mvarqAutoEnableIO = lens _mvarqAutoEnableIO (\ s a -> s{_mvarqAutoEnableIO = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mvarqDryRun :: Lens' ModifyVolumeAttribute (Maybe Bool)
mvarqDryRun = lens _mvarqDryRun (\ s a -> s{_mvarqDryRun = a});

-- | The ID of the volume.
mvarqVolumeId :: Lens' ModifyVolumeAttribute Text
mvarqVolumeId = lens _mvarqVolumeId (\ s a -> s{_mvarqVolumeId = a});

instance AWSRequest ModifyVolumeAttribute where
        type Sv ModifyVolumeAttribute = EC2
        type Rs ModifyVolumeAttribute =
             ModifyVolumeAttributeResponse
        request = post
        response = receiveNull ModifyVolumeAttributeResponse'

instance ToHeaders ModifyVolumeAttribute where
        toHeaders = const mempty

instance ToPath ModifyVolumeAttribute where
        toPath = const "/"

instance ToQuery ModifyVolumeAttribute where
        toQuery ModifyVolumeAttribute'{..}
          = mconcat
              ["Action" =: ("ModifyVolumeAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AutoEnableIO" =: _mvarqAutoEnableIO,
               "DryRun" =: _mvarqDryRun,
               "VolumeId" =: _mvarqVolumeId]

-- | /See:/ 'modifyVolumeAttributeResponse' smart constructor.
data ModifyVolumeAttributeResponse =
    ModifyVolumeAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVolumeAttributeResponse' smart constructor.
modifyVolumeAttributeResponse :: ModifyVolumeAttributeResponse
modifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'
