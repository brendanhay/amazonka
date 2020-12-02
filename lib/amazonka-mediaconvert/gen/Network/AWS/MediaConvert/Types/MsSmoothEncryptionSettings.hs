{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.Prelude

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
--
-- /See:/ 'msSmoothEncryptionSettings' smart constructor.
newtype MsSmoothEncryptionSettings = MsSmoothEncryptionSettings'
  { _msesSpekeKeyProvider ::
      Maybe SpekeKeyProvider
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MsSmoothEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msesSpekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
msSmoothEncryptionSettings ::
  MsSmoothEncryptionSettings
msSmoothEncryptionSettings =
  MsSmoothEncryptionSettings' {_msesSpekeKeyProvider = Nothing}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
msesSpekeKeyProvider :: Lens' MsSmoothEncryptionSettings (Maybe SpekeKeyProvider)
msesSpekeKeyProvider = lens _msesSpekeKeyProvider (\s a -> s {_msesSpekeKeyProvider = a})

instance FromJSON MsSmoothEncryptionSettings where
  parseJSON =
    withObject
      "MsSmoothEncryptionSettings"
      (\x -> MsSmoothEncryptionSettings' <$> (x .:? "spekeKeyProvider"))

instance Hashable MsSmoothEncryptionSettings

instance NFData MsSmoothEncryptionSettings

instance ToJSON MsSmoothEncryptionSettings where
  toJSON MsSmoothEncryptionSettings' {..} =
    object
      (catMaybes [("spekeKeyProvider" .=) <$> _msesSpekeKeyProvider])
