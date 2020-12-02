{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IKEVersionsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IKEVersionsRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The IKE version that is permitted for the VPN tunnel.
--
--
--
-- /See:/ 'iKEVersionsRequestListValue' smart constructor.
newtype IKEVersionsRequestListValue = IKEVersionsRequestListValue'
  { _ikevrlvValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IKEVersionsRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikevrlvValue' - The IKE version.
iKEVersionsRequestListValue ::
  IKEVersionsRequestListValue
iKEVersionsRequestListValue =
  IKEVersionsRequestListValue' {_ikevrlvValue = Nothing}

-- | The IKE version.
ikevrlvValue :: Lens' IKEVersionsRequestListValue (Maybe Text)
ikevrlvValue = lens _ikevrlvValue (\s a -> s {_ikevrlvValue = a})

instance Hashable IKEVersionsRequestListValue

instance NFData IKEVersionsRequestListValue

instance ToQuery IKEVersionsRequestListValue where
  toQuery IKEVersionsRequestListValue' {..} =
    mconcat ["Value" =: _ikevrlvValue]
