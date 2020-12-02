{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits

-- | Limits settings by pattern type in the protection groups for your subscription.
--
--
--
-- /See:/ 'protectionGroupPatternTypeLimits' smart constructor.
newtype ProtectionGroupPatternTypeLimits = ProtectionGroupPatternTypeLimits'
  { _pgptlArbitraryPatternLimits ::
      ProtectionGroupArbitraryPatternLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectionGroupPatternTypeLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgptlArbitraryPatternLimits' - Limits settings on protection groups with arbitrary pattern type.
protectionGroupPatternTypeLimits ::
  -- | 'pgptlArbitraryPatternLimits'
  ProtectionGroupArbitraryPatternLimits ->
  ProtectionGroupPatternTypeLimits
protectionGroupPatternTypeLimits pArbitraryPatternLimits_ =
  ProtectionGroupPatternTypeLimits'
    { _pgptlArbitraryPatternLimits =
        pArbitraryPatternLimits_
    }

-- | Limits settings on protection groups with arbitrary pattern type.
pgptlArbitraryPatternLimits :: Lens' ProtectionGroupPatternTypeLimits ProtectionGroupArbitraryPatternLimits
pgptlArbitraryPatternLimits = lens _pgptlArbitraryPatternLimits (\s a -> s {_pgptlArbitraryPatternLimits = a})

instance FromJSON ProtectionGroupPatternTypeLimits where
  parseJSON =
    withObject
      "ProtectionGroupPatternTypeLimits"
      ( \x ->
          ProtectionGroupPatternTypeLimits'
            <$> (x .: "ArbitraryPatternLimits")
      )

instance Hashable ProtectionGroupPatternTypeLimits

instance NFData ProtectionGroupPatternTypeLimits
