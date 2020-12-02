{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.VaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultAccessPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the vault access policy.
--
--
--
-- /See:/ 'vaultAccessPolicy' smart constructor.
newtype VaultAccessPolicy = VaultAccessPolicy'
  { _vapPolicy ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vapPolicy' - The vault access policy.
vaultAccessPolicy ::
  VaultAccessPolicy
vaultAccessPolicy = VaultAccessPolicy' {_vapPolicy = Nothing}

-- | The vault access policy.
vapPolicy :: Lens' VaultAccessPolicy (Maybe Text)
vapPolicy = lens _vapPolicy (\s a -> s {_vapPolicy = a})

instance FromJSON VaultAccessPolicy where
  parseJSON =
    withObject
      "VaultAccessPolicy"
      (\x -> VaultAccessPolicy' <$> (x .:? "Policy"))

instance Hashable VaultAccessPolicy

instance NFData VaultAccessPolicy

instance ToJSON VaultAccessPolicy where
  toJSON VaultAccessPolicy' {..} =
    object (catMaybes [("Policy" .=) <$> _vapPolicy])
