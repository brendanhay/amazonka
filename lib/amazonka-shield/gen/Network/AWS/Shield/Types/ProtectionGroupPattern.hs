{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPattern
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupPattern
  ( ProtectionGroupPattern
      ( ProtectionGroupPattern',
        ProtectionGroupPatternAll,
        ProtectionGroupPatternArbitrary,
        ProtectionGroupPatternByResourceType,
        fromProtectionGroupPattern
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProtectionGroupPattern = ProtectionGroupPattern'
  { fromProtectionGroupPattern ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ProtectionGroupPatternAll :: ProtectionGroupPattern
pattern ProtectionGroupPatternAll = ProtectionGroupPattern' "ALL"

pattern ProtectionGroupPatternArbitrary :: ProtectionGroupPattern
pattern ProtectionGroupPatternArbitrary = ProtectionGroupPattern' "ARBITRARY"

pattern ProtectionGroupPatternByResourceType :: ProtectionGroupPattern
pattern ProtectionGroupPatternByResourceType = ProtectionGroupPattern' "BY_RESOURCE_TYPE"

{-# COMPLETE
  ProtectionGroupPatternAll,
  ProtectionGroupPatternArbitrary,
  ProtectionGroupPatternByResourceType,
  ProtectionGroupPattern'
  #-}
