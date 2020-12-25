{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
  ( UpdateTargetsArchitecture
      ( UpdateTargetsArchitecture',
        UpdateTargetsArchitectureArmv6l,
        UpdateTargetsArchitectureArmv7l,
        UpdateTargetsArchitectureX8664,
        UpdateTargetsArchitectureAARCH64,
        fromUpdateTargetsArchitecture
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The architecture of the cores which are the targets of an update.
newtype UpdateTargetsArchitecture = UpdateTargetsArchitecture'
  { fromUpdateTargetsArchitecture ::
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

pattern UpdateTargetsArchitectureArmv6l :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitectureArmv6l = UpdateTargetsArchitecture' "armv6l"

pattern UpdateTargetsArchitectureArmv7l :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitectureArmv7l = UpdateTargetsArchitecture' "armv7l"

pattern UpdateTargetsArchitectureX8664 :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitectureX8664 = UpdateTargetsArchitecture' "x86_64"

pattern UpdateTargetsArchitectureAARCH64 :: UpdateTargetsArchitecture
pattern UpdateTargetsArchitectureAARCH64 = UpdateTargetsArchitecture' "aarch64"

{-# COMPLETE
  UpdateTargetsArchitectureArmv6l,
  UpdateTargetsArchitectureArmv7l,
  UpdateTargetsArchitectureX8664,
  UpdateTargetsArchitectureAARCH64,
  UpdateTargetsArchitecture'
  #-}
