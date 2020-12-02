{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputWhitelistRuleCidr where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An IPv4 CIDR to whitelist.
--
-- /See:/ 'inputWhitelistRuleCidr' smart constructor.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr'
  { _iwrcCidr ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputWhitelistRuleCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iwrcCidr' - The IPv4 CIDR to whitelist.
inputWhitelistRuleCidr ::
  InputWhitelistRuleCidr
inputWhitelistRuleCidr =
  InputWhitelistRuleCidr' {_iwrcCidr = Nothing}

-- | The IPv4 CIDR to whitelist.
iwrcCidr :: Lens' InputWhitelistRuleCidr (Maybe Text)
iwrcCidr = lens _iwrcCidr (\s a -> s {_iwrcCidr = a})

instance Hashable InputWhitelistRuleCidr

instance NFData InputWhitelistRuleCidr

instance ToJSON InputWhitelistRuleCidr where
  toJSON InputWhitelistRuleCidr' {..} =
    object (catMaybes [("cidr" .=) <$> _iwrcCidr])
