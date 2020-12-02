{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.MssPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.MssPackage where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.MssEncryption
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.Prelude

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'mssPackage' smart constructor.
data MssPackage = MssPackage'
  { _mpSegmentDurationSeconds ::
      !(Maybe Int),
    _mpStreamSelection :: !(Maybe StreamSelection),
    _mpEncryption :: !(Maybe MssEncryption),
    _mpManifestWindowSeconds :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MssPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpSegmentDurationSeconds' - The duration (in seconds) of each segment.
--
-- * 'mpStreamSelection' - Undocumented member.
--
-- * 'mpEncryption' - Undocumented member.
--
-- * 'mpManifestWindowSeconds' - The time window (in seconds) contained in each manifest.
mssPackage ::
  MssPackage
mssPackage =
  MssPackage'
    { _mpSegmentDurationSeconds = Nothing,
      _mpStreamSelection = Nothing,
      _mpEncryption = Nothing,
      _mpManifestWindowSeconds = Nothing
    }

-- | The duration (in seconds) of each segment.
mpSegmentDurationSeconds :: Lens' MssPackage (Maybe Int)
mpSegmentDurationSeconds = lens _mpSegmentDurationSeconds (\s a -> s {_mpSegmentDurationSeconds = a})

-- | Undocumented member.
mpStreamSelection :: Lens' MssPackage (Maybe StreamSelection)
mpStreamSelection = lens _mpStreamSelection (\s a -> s {_mpStreamSelection = a})

-- | Undocumented member.
mpEncryption :: Lens' MssPackage (Maybe MssEncryption)
mpEncryption = lens _mpEncryption (\s a -> s {_mpEncryption = a})

-- | The time window (in seconds) contained in each manifest.
mpManifestWindowSeconds :: Lens' MssPackage (Maybe Int)
mpManifestWindowSeconds = lens _mpManifestWindowSeconds (\s a -> s {_mpManifestWindowSeconds = a})

instance FromJSON MssPackage where
  parseJSON =
    withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            <$> (x .:? "segmentDurationSeconds")
            <*> (x .:? "streamSelection")
            <*> (x .:? "encryption")
            <*> (x .:? "manifestWindowSeconds")
      )

instance Hashable MssPackage

instance NFData MssPackage

instance ToJSON MssPackage where
  toJSON MssPackage' {..} =
    object
      ( catMaybes
          [ ("segmentDurationSeconds" .=) <$> _mpSegmentDurationSeconds,
            ("streamSelection" .=) <$> _mpStreamSelection,
            ("encryption" .=) <$> _mpEncryption,
            ("manifestWindowSeconds" .=) <$> _mpManifestWindowSeconds
          ]
      )
