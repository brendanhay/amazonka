{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.Prelude

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'cmafPackageCreateOrUpdateParameters' smart constructor.
data CmafPackageCreateOrUpdateParameters = CmafPackageCreateOrUpdateParameters'
  { _cpcoupHlsManifests ::
      !( Maybe
           [HlsManifestCreateOrUpdateParameters]
       ),
    _cpcoupSegmentDurationSeconds ::
      !(Maybe Int),
    _cpcoupStreamSelection ::
      !( Maybe
           StreamSelection
       ),
    _cpcoupEncryption ::
      !( Maybe
           CmafEncryption
       ),
    _cpcoupSegmentPrefix ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CmafPackageCreateOrUpdateParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcoupHlsManifests' - A list of HLS manifest configurations
--
-- * 'cpcoupSegmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
--
-- * 'cpcoupStreamSelection' - Undocumented member.
--
-- * 'cpcoupEncryption' - Undocumented member.
--
-- * 'cpcoupSegmentPrefix' - An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
cmafPackageCreateOrUpdateParameters ::
  CmafPackageCreateOrUpdateParameters
cmafPackageCreateOrUpdateParameters =
  CmafPackageCreateOrUpdateParameters'
    { _cpcoupHlsManifests =
        Nothing,
      _cpcoupSegmentDurationSeconds = Nothing,
      _cpcoupStreamSelection = Nothing,
      _cpcoupEncryption = Nothing,
      _cpcoupSegmentPrefix = Nothing
    }

-- | A list of HLS manifest configurations
cpcoupHlsManifests :: Lens' CmafPackageCreateOrUpdateParameters [HlsManifestCreateOrUpdateParameters]
cpcoupHlsManifests = lens _cpcoupHlsManifests (\s a -> s {_cpcoupHlsManifests = a}) . _Default . _Coerce

-- | Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
cpcoupSegmentDurationSeconds :: Lens' CmafPackageCreateOrUpdateParameters (Maybe Int)
cpcoupSegmentDurationSeconds = lens _cpcoupSegmentDurationSeconds (\s a -> s {_cpcoupSegmentDurationSeconds = a})

-- | Undocumented member.
cpcoupStreamSelection :: Lens' CmafPackageCreateOrUpdateParameters (Maybe StreamSelection)
cpcoupStreamSelection = lens _cpcoupStreamSelection (\s a -> s {_cpcoupStreamSelection = a})

-- | Undocumented member.
cpcoupEncryption :: Lens' CmafPackageCreateOrUpdateParameters (Maybe CmafEncryption)
cpcoupEncryption = lens _cpcoupEncryption (\s a -> s {_cpcoupEncryption = a})

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
cpcoupSegmentPrefix :: Lens' CmafPackageCreateOrUpdateParameters (Maybe Text)
cpcoupSegmentPrefix = lens _cpcoupSegmentPrefix (\s a -> s {_cpcoupSegmentPrefix = a})

instance Hashable CmafPackageCreateOrUpdateParameters

instance NFData CmafPackageCreateOrUpdateParameters

instance ToJSON CmafPackageCreateOrUpdateParameters where
  toJSON CmafPackageCreateOrUpdateParameters' {..} =
    object
      ( catMaybes
          [ ("hlsManifests" .=) <$> _cpcoupHlsManifests,
            ("segmentDurationSeconds" .=) <$> _cpcoupSegmentDurationSeconds,
            ("streamSelection" .=) <$> _cpcoupStreamSelection,
            ("encryption" .=) <$> _cpcoupEncryption,
            ("segmentPrefix" .=) <$> _cpcoupSegmentPrefix
          ]
      )
