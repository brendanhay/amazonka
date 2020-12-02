{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Limits settings on protection groups with arbitrary pattern type.
--
--
--
-- /See:/ 'protectionGroupArbitraryPatternLimits' smart constructor.
newtype ProtectionGroupArbitraryPatternLimits = ProtectionGroupArbitraryPatternLimits'
  { _pgaplMaxMembers ::
      Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectionGroupArbitraryPatternLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgaplMaxMembers' - The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
protectionGroupArbitraryPatternLimits ::
  -- | 'pgaplMaxMembers'
  Integer ->
  ProtectionGroupArbitraryPatternLimits
protectionGroupArbitraryPatternLimits pMaxMembers_ =
  ProtectionGroupArbitraryPatternLimits'
    { _pgaplMaxMembers =
        pMaxMembers_
    }

-- | The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
pgaplMaxMembers :: Lens' ProtectionGroupArbitraryPatternLimits Integer
pgaplMaxMembers = lens _pgaplMaxMembers (\s a -> s {_pgaplMaxMembers = a})

instance FromJSON ProtectionGroupArbitraryPatternLimits where
  parseJSON =
    withObject
      "ProtectionGroupArbitraryPatternLimits"
      ( \x ->
          ProtectionGroupArbitraryPatternLimits' <$> (x .: "MaxMembers")
      )

instance Hashable ProtectionGroupArbitraryPatternLimits

instance NFData ProtectionGroupArbitraryPatternLimits
