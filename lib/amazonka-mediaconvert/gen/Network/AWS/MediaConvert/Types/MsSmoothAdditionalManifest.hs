{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specify the details for each additional Microsoft Smooth Streaming manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'msSmoothAdditionalManifest' smart constructor.
data MsSmoothAdditionalManifest = MsSmoothAdditionalManifest'
  { _msamManifestNameModifier ::
      !(Maybe Text),
    _msamSelectedOutputs ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MsSmoothAdditionalManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msamManifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your Microsoft Smooth group is film-name.ismv. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.ismv.
--
-- * 'msamSelectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
msSmoothAdditionalManifest ::
  MsSmoothAdditionalManifest
msSmoothAdditionalManifest =
  MsSmoothAdditionalManifest'
    { _msamManifestNameModifier = Nothing,
      _msamSelectedOutputs = Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your Microsoft Smooth group is film-name.ismv. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.ismv.
msamManifestNameModifier :: Lens' MsSmoothAdditionalManifest (Maybe Text)
msamManifestNameModifier = lens _msamManifestNameModifier (\s a -> s {_msamManifestNameModifier = a})

-- | Specify the outputs that you want this additional top-level manifest to reference.
msamSelectedOutputs :: Lens' MsSmoothAdditionalManifest [Text]
msamSelectedOutputs = lens _msamSelectedOutputs (\s a -> s {_msamSelectedOutputs = a}) . _Default . _Coerce

instance FromJSON MsSmoothAdditionalManifest where
  parseJSON =
    withObject
      "MsSmoothAdditionalManifest"
      ( \x ->
          MsSmoothAdditionalManifest'
            <$> (x .:? "manifestNameModifier")
            <*> (x .:? "selectedOutputs" .!= mempty)
      )

instance Hashable MsSmoothAdditionalManifest

instance NFData MsSmoothAdditionalManifest

instance ToJSON MsSmoothAdditionalManifest where
  toJSON MsSmoothAdditionalManifest' {..} =
    object
      ( catMaybes
          [ ("manifestNameModifier" .=) <$> _msamManifestNameModifier,
            ("selectedOutputs" .=) <$> _msamSelectedOutputs
          ]
      )
