{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IKEVersionsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IKEVersionsListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The internet key exchange (IKE) version permitted for the VPN tunnel.
--
--
--
-- /See:/ 'iKEVersionsListValue' smart constructor.
newtype IKEVersionsListValue = IKEVersionsListValue'
  { _ikevlvValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IKEVersionsListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikevlvValue' - The IKE version.
iKEVersionsListValue ::
  IKEVersionsListValue
iKEVersionsListValue =
  IKEVersionsListValue' {_ikevlvValue = Nothing}

-- | The IKE version.
ikevlvValue :: Lens' IKEVersionsListValue (Maybe Text)
ikevlvValue = lens _ikevlvValue (\s a -> s {_ikevlvValue = a})

instance FromXML IKEVersionsListValue where
  parseXML x = IKEVersionsListValue' <$> (x .@? "value")

instance Hashable IKEVersionsListValue

instance NFData IKEVersionsListValue
