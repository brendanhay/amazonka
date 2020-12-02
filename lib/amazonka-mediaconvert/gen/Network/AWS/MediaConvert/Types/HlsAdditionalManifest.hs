{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAdditionalManifest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specify the details for each additional HLS manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'hlsAdditionalManifest' smart constructor.
data HlsAdditionalManifest = HlsAdditionalManifest'
  { _hamManifestNameModifier ::
      !(Maybe Text),
    _hamSelectedOutputs :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsAdditionalManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hamManifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
--
-- * 'hamSelectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
hlsAdditionalManifest ::
  HlsAdditionalManifest
hlsAdditionalManifest =
  HlsAdditionalManifest'
    { _hamManifestNameModifier = Nothing,
      _hamSelectedOutputs = Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
hamManifestNameModifier :: Lens' HlsAdditionalManifest (Maybe Text)
hamManifestNameModifier = lens _hamManifestNameModifier (\s a -> s {_hamManifestNameModifier = a})

-- | Specify the outputs that you want this additional top-level manifest to reference.
hamSelectedOutputs :: Lens' HlsAdditionalManifest [Text]
hamSelectedOutputs = lens _hamSelectedOutputs (\s a -> s {_hamSelectedOutputs = a}) . _Default . _Coerce

instance FromJSON HlsAdditionalManifest where
  parseJSON =
    withObject
      "HlsAdditionalManifest"
      ( \x ->
          HlsAdditionalManifest'
            <$> (x .:? "manifestNameModifier")
            <*> (x .:? "selectedOutputs" .!= mempty)
      )

instance Hashable HlsAdditionalManifest

instance NFData HlsAdditionalManifest

instance ToJSON HlsAdditionalManifest where
  toJSON HlsAdditionalManifest' {..} =
    object
      ( catMaybes
          [ ("manifestNameModifier" .=) <$> _hamManifestNameModifier,
            ("selectedOutputs" .=) <$> _hamSelectedOutputs
          ]
      )
