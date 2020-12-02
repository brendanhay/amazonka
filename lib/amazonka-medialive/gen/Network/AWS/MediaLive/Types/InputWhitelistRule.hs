{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputWhitelistRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Whitelist rule
--
-- /See:/ 'inputWhitelistRule' smart constructor.
newtype InputWhitelistRule = InputWhitelistRule'
  { _iwrCidr ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputWhitelistRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iwrCidr' - The IPv4 CIDR that's whitelisted.
inputWhitelistRule ::
  InputWhitelistRule
inputWhitelistRule = InputWhitelistRule' {_iwrCidr = Nothing}

-- | The IPv4 CIDR that's whitelisted.
iwrCidr :: Lens' InputWhitelistRule (Maybe Text)
iwrCidr = lens _iwrCidr (\s a -> s {_iwrCidr = a})

instance FromJSON InputWhitelistRule where
  parseJSON =
    withObject
      "InputWhitelistRule"
      (\x -> InputWhitelistRule' <$> (x .:? "cidr"))

instance Hashable InputWhitelistRule

instance NFData InputWhitelistRule
