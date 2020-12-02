{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PartnerWatermarking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PartnerWatermarking where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
import Network.AWS.Prelude

-- | If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
--
-- /See:/ 'partnerWatermarking' smart constructor.
newtype PartnerWatermarking = PartnerWatermarking'
  { _pwNexguardFileMarkerSettings ::
      Maybe NexGuardFileMarkerSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartnerWatermarking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwNexguardFileMarkerSettings' - For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
partnerWatermarking ::
  PartnerWatermarking
partnerWatermarking =
  PartnerWatermarking' {_pwNexguardFileMarkerSettings = Nothing}

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
pwNexguardFileMarkerSettings :: Lens' PartnerWatermarking (Maybe NexGuardFileMarkerSettings)
pwNexguardFileMarkerSettings = lens _pwNexguardFileMarkerSettings (\s a -> s {_pwNexguardFileMarkerSettings = a})

instance FromJSON PartnerWatermarking where
  parseJSON =
    withObject
      "PartnerWatermarking"
      ( \x ->
          PartnerWatermarking' <$> (x .:? "nexguardFileMarkerSettings")
      )

instance Hashable PartnerWatermarking

instance NFData PartnerWatermarking

instance ToJSON PartnerWatermarking where
  toJSON PartnerWatermarking' {..} =
    object
      ( catMaybes
          [ ("nexguardFileMarkerSettings" .=)
              <$> _pwNexguardFileMarkerSettings
          ]
      )
