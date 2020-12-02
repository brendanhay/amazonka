{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupLimits where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits

-- | Limits settings on protection groups for your subscription.
--
--
--
-- /See:/ 'protectionGroupLimits' smart constructor.
data ProtectionGroupLimits = ProtectionGroupLimits'
  { _pglMaxProtectionGroups ::
      !Integer,
    _pglPatternTypeLimits ::
      !ProtectionGroupPatternTypeLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectionGroupLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pglMaxProtectionGroups' - The maximum number of protection groups that you can have at one time.
--
-- * 'pglPatternTypeLimits' - Limits settings by pattern type in the protection groups for your subscription.
protectionGroupLimits ::
  -- | 'pglMaxProtectionGroups'
  Integer ->
  -- | 'pglPatternTypeLimits'
  ProtectionGroupPatternTypeLimits ->
  ProtectionGroupLimits
protectionGroupLimits pMaxProtectionGroups_ pPatternTypeLimits_ =
  ProtectionGroupLimits'
    { _pglMaxProtectionGroups =
        pMaxProtectionGroups_,
      _pglPatternTypeLimits = pPatternTypeLimits_
    }

-- | The maximum number of protection groups that you can have at one time.
pglMaxProtectionGroups :: Lens' ProtectionGroupLimits Integer
pglMaxProtectionGroups = lens _pglMaxProtectionGroups (\s a -> s {_pglMaxProtectionGroups = a})

-- | Limits settings by pattern type in the protection groups for your subscription.
pglPatternTypeLimits :: Lens' ProtectionGroupLimits ProtectionGroupPatternTypeLimits
pglPatternTypeLimits = lens _pglPatternTypeLimits (\s a -> s {_pglPatternTypeLimits = a})

instance FromJSON ProtectionGroupLimits where
  parseJSON =
    withObject
      "ProtectionGroupLimits"
      ( \x ->
          ProtectionGroupLimits'
            <$> (x .: "MaxProtectionGroups") <*> (x .: "PatternTypeLimits")
      )

instance Hashable ProtectionGroupLimits

instance NFData ProtectionGroupLimits
