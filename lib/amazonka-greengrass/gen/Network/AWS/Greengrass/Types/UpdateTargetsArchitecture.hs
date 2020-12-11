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
        AARCH64,
        Armv6l,
        Armv7l,
        X86_64
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The architecture of the cores which are the targets of an update.
newtype UpdateTargetsArchitecture = UpdateTargetsArchitecture' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AARCH64 :: UpdateTargetsArchitecture
pattern AARCH64 = UpdateTargetsArchitecture' "aarch64"

pattern Armv6l :: UpdateTargetsArchitecture
pattern Armv6l = UpdateTargetsArchitecture' "armv6l"

pattern Armv7l :: UpdateTargetsArchitecture
pattern Armv7l = UpdateTargetsArchitecture' "armv7l"

pattern X86_64 :: UpdateTargetsArchitecture
pattern X86_64 = UpdateTargetsArchitecture' "x86_64"

{-# COMPLETE
  AARCH64,
  Armv6l,
  Armv7l,
  X86_64,
  UpdateTargetsArchitecture'
  #-}
