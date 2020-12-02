{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAttachmentStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the instances to which the volume is attached.
--
--
--
-- /See:/ 'volumeStatusAttachmentStatus' smart constructor.
data VolumeStatusAttachmentStatus = VolumeStatusAttachmentStatus'
  { _vsasInstanceId ::
      !(Maybe Text),
    _vsasIOPerformance ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeStatusAttachmentStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsasInstanceId' - The ID of the attached instance.
--
-- * 'vsasIOPerformance' - The maximum IOPS supported by the attached instance.
volumeStatusAttachmentStatus ::
  VolumeStatusAttachmentStatus
volumeStatusAttachmentStatus =
  VolumeStatusAttachmentStatus'
    { _vsasInstanceId = Nothing,
      _vsasIOPerformance = Nothing
    }

-- | The ID of the attached instance.
vsasInstanceId :: Lens' VolumeStatusAttachmentStatus (Maybe Text)
vsasInstanceId = lens _vsasInstanceId (\s a -> s {_vsasInstanceId = a})

-- | The maximum IOPS supported by the attached instance.
vsasIOPerformance :: Lens' VolumeStatusAttachmentStatus (Maybe Text)
vsasIOPerformance = lens _vsasIOPerformance (\s a -> s {_vsasIOPerformance = a})

instance FromXML VolumeStatusAttachmentStatus where
  parseXML x =
    VolumeStatusAttachmentStatus'
      <$> (x .@? "instanceId") <*> (x .@? "ioPerformance")

instance Hashable VolumeStatusAttachmentStatus

instance NFData VolumeStatusAttachmentStatus
