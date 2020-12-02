{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.DashEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.DashEncryption where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import Network.AWS.Prelude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
--
-- /See:/ 'dashEncryption' smart constructor.
data DashEncryption = DashEncryption'
  { _deKeyRotationIntervalSeconds ::
      !(Maybe Int),
    _deSpekeKeyProvider :: !SpekeKeyProvider
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DashEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deKeyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
--
-- * 'deSpekeKeyProvider' - Undocumented member.
dashEncryption ::
  -- | 'deSpekeKeyProvider'
  SpekeKeyProvider ->
  DashEncryption
dashEncryption pSpekeKeyProvider_ =
  DashEncryption'
    { _deKeyRotationIntervalSeconds = Nothing,
      _deSpekeKeyProvider = pSpekeKeyProvider_
    }

-- | Time (in seconds) between each encryption key rotation.
deKeyRotationIntervalSeconds :: Lens' DashEncryption (Maybe Int)
deKeyRotationIntervalSeconds = lens _deKeyRotationIntervalSeconds (\s a -> s {_deKeyRotationIntervalSeconds = a})

-- | Undocumented member.
deSpekeKeyProvider :: Lens' DashEncryption SpekeKeyProvider
deSpekeKeyProvider = lens _deSpekeKeyProvider (\s a -> s {_deSpekeKeyProvider = a})

instance FromJSON DashEncryption where
  parseJSON =
    withObject
      "DashEncryption"
      ( \x ->
          DashEncryption'
            <$> (x .:? "keyRotationIntervalSeconds") <*> (x .: "spekeKeyProvider")
      )

instance Hashable DashEncryption

instance NFData DashEncryption

instance ToJSON DashEncryption where
  toJSON DashEncryption' {..} =
    object
      ( catMaybes
          [ ("keyRotationIntervalSeconds" .=)
              <$> _deKeyRotationIntervalSeconds,
            Just ("spekeKeyProvider" .= _deSpekeKeyProvider)
          ]
      )
