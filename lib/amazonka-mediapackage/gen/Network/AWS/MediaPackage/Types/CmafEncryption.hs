{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafEncryption where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import Network.AWS.Prelude

-- | A Common Media Application Format (CMAF) encryption configuration.
--
-- /See:/ 'cmafEncryption' smart constructor.
data CmafEncryption = CmafEncryption'
  { _ceKeyRotationIntervalSeconds ::
      !(Maybe Int),
    _ceSpekeKeyProvider :: !SpekeKeyProvider
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CmafEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceKeyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
--
-- * 'ceSpekeKeyProvider' - Undocumented member.
cmafEncryption ::
  -- | 'ceSpekeKeyProvider'
  SpekeKeyProvider ->
  CmafEncryption
cmafEncryption pSpekeKeyProvider_ =
  CmafEncryption'
    { _ceKeyRotationIntervalSeconds = Nothing,
      _ceSpekeKeyProvider = pSpekeKeyProvider_
    }

-- | Time (in seconds) between each encryption key rotation.
ceKeyRotationIntervalSeconds :: Lens' CmafEncryption (Maybe Int)
ceKeyRotationIntervalSeconds = lens _ceKeyRotationIntervalSeconds (\s a -> s {_ceKeyRotationIntervalSeconds = a})

-- | Undocumented member.
ceSpekeKeyProvider :: Lens' CmafEncryption SpekeKeyProvider
ceSpekeKeyProvider = lens _ceSpekeKeyProvider (\s a -> s {_ceSpekeKeyProvider = a})

instance FromJSON CmafEncryption where
  parseJSON =
    withObject
      "CmafEncryption"
      ( \x ->
          CmafEncryption'
            <$> (x .:? "keyRotationIntervalSeconds") <*> (x .: "spekeKeyProvider")
      )

instance Hashable CmafEncryption

instance NFData CmafEncryption

instance ToJSON CmafEncryption where
  toJSON CmafEncryption' {..} =
    object
      ( catMaybes
          [ ("keyRotationIntervalSeconds" .=)
              <$> _ceKeyRotationIntervalSeconds,
            Just ("spekeKeyProvider" .= _ceSpekeKeyProvider)
          ]
      )
