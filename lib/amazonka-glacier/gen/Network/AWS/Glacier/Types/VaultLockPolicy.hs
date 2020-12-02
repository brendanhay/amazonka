{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.VaultLockPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultLockPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the vault lock policy.
--
--
--
-- /See:/ 'vaultLockPolicy' smart constructor.
newtype VaultLockPolicy = VaultLockPolicy'
  { _vlpPolicy ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VaultLockPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vlpPolicy' - The vault lock policy.
vaultLockPolicy ::
  VaultLockPolicy
vaultLockPolicy = VaultLockPolicy' {_vlpPolicy = Nothing}

-- | The vault lock policy.
vlpPolicy :: Lens' VaultLockPolicy (Maybe Text)
vlpPolicy = lens _vlpPolicy (\s a -> s {_vlpPolicy = a})

instance Hashable VaultLockPolicy

instance NFData VaultLockPolicy

instance ToJSON VaultLockPolicy where
  toJSON VaultLockPolicy' {..} =
    object (catMaybes [("Policy" .=) <$> _vlpPolicy])
