{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafAdditionalManifest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specify the details for each pair of HLS and DASH additional manifests that you want the service to generate for this CMAF output group. Each pair of manifests can reference a different subset of outputs in the group.
--
-- /See:/ 'cmafAdditionalManifest' smart constructor.
data CmafAdditionalManifest = CmafAdditionalManifest'
  { _camManifestNameModifier ::
      !(Maybe Text),
    _camSelectedOutputs :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CmafAdditionalManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camManifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
--
-- * 'camSelectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
cmafAdditionalManifest ::
  CmafAdditionalManifest
cmafAdditionalManifest =
  CmafAdditionalManifest'
    { _camManifestNameModifier = Nothing,
      _camSelectedOutputs = Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your HLS group is film-name.m3u8. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.m3u8. For HLS output groups, specify a manifestNameModifier that is different from the nameModifier of the output. The service uses the output name modifier to create unique names for the individual variant manifests.
camManifestNameModifier :: Lens' CmafAdditionalManifest (Maybe Text)
camManifestNameModifier = lens _camManifestNameModifier (\s a -> s {_camManifestNameModifier = a})

-- | Specify the outputs that you want this additional top-level manifest to reference.
camSelectedOutputs :: Lens' CmafAdditionalManifest [Text]
camSelectedOutputs = lens _camSelectedOutputs (\s a -> s {_camSelectedOutputs = a}) . _Default . _Coerce

instance FromJSON CmafAdditionalManifest where
  parseJSON =
    withObject
      "CmafAdditionalManifest"
      ( \x ->
          CmafAdditionalManifest'
            <$> (x .:? "manifestNameModifier")
            <*> (x .:? "selectedOutputs" .!= mempty)
      )

instance Hashable CmafAdditionalManifest

instance NFData CmafAdditionalManifest

instance ToJSON CmafAdditionalManifest where
  toJSON CmafAdditionalManifest' {..} =
    object
      ( catMaybes
          [ ("manifestNameModifier" .=) <$> _camManifestNameModifier,
            ("selectedOutputs" .=) <$> _camSelectedOutputs
          ]
      )
