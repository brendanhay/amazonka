{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.MssEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.MssEncryption where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import Network.AWS.Prelude

-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
--
-- /See:/ 'mssEncryption' smart constructor.
newtype MssEncryption = MssEncryption'
  { _meSpekeKeyProvider ::
      SpekeKeyProvider
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MssEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meSpekeKeyProvider' - Undocumented member.
mssEncryption ::
  -- | 'meSpekeKeyProvider'
  SpekeKeyProvider ->
  MssEncryption
mssEncryption pSpekeKeyProvider_ =
  MssEncryption' {_meSpekeKeyProvider = pSpekeKeyProvider_}

-- | Undocumented member.
meSpekeKeyProvider :: Lens' MssEncryption SpekeKeyProvider
meSpekeKeyProvider = lens _meSpekeKeyProvider (\s a -> s {_meSpekeKeyProvider = a})

instance FromJSON MssEncryption where
  parseJSON =
    withObject
      "MssEncryption"
      (\x -> MssEncryption' <$> (x .: "spekeKeyProvider"))

instance Hashable MssEncryption

instance NFData MssEncryption

instance ToJSON MssEncryption where
  toJSON MssEncryption' {..} =
    object
      (catMaybes [Just ("spekeKeyProvider" .= _meSpekeKeyProvider)])
