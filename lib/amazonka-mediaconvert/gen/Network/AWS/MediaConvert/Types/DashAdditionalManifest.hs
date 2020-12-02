{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashAdditionalManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashAdditionalManifest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specify the details for each additional DASH manifest that you want the service to generate for this output group. Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'dashAdditionalManifest' smart constructor.
data DashAdditionalManifest = DashAdditionalManifest'
  { _damManifestNameModifier ::
      !(Maybe Text),
    _damSelectedOutputs :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DashAdditionalManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damManifestNameModifier' - Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your DASH group is film-name.mpd. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.mpd.
--
-- * 'damSelectedOutputs' - Specify the outputs that you want this additional top-level manifest to reference.
dashAdditionalManifest ::
  DashAdditionalManifest
dashAdditionalManifest =
  DashAdditionalManifest'
    { _damManifestNameModifier = Nothing,
      _damSelectedOutputs = Nothing
    }

-- | Specify a name modifier that the service adds to the name of this manifest to make it different from the file names of the other main manifests in the output group. For example, say that the default main manifest for your DASH group is film-name.mpd. If you enter "-no-premium" for this setting, then the file name the service generates for this top-level manifest is film-name-no-premium.mpd.
damManifestNameModifier :: Lens' DashAdditionalManifest (Maybe Text)
damManifestNameModifier = lens _damManifestNameModifier (\s a -> s {_damManifestNameModifier = a})

-- | Specify the outputs that you want this additional top-level manifest to reference.
damSelectedOutputs :: Lens' DashAdditionalManifest [Text]
damSelectedOutputs = lens _damSelectedOutputs (\s a -> s {_damSelectedOutputs = a}) . _Default . _Coerce

instance FromJSON DashAdditionalManifest where
  parseJSON =
    withObject
      "DashAdditionalManifest"
      ( \x ->
          DashAdditionalManifest'
            <$> (x .:? "manifestNameModifier")
            <*> (x .:? "selectedOutputs" .!= mempty)
      )

instance Hashable DashAdditionalManifest

instance NFData DashAdditionalManifest

instance ToJSON DashAdditionalManifest where
  toJSON DashAdditionalManifest' {..} =
    object
      ( catMaybes
          [ ("manifestNameModifier" .=) <$> _damManifestNameModifier,
            ("selectedOutputs" .=) <$> _damSelectedOutputs
          ]
      )
