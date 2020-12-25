{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsKlv
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsKlv
  ( M2tsKlv
      ( M2tsKlv',
        M2tsKlvNone,
        M2tsKlvPassthrough,
        fromM2tsKlv
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M2ts Klv
newtype M2tsKlv = M2tsKlv' {fromM2tsKlv :: Core.Text}
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

pattern M2tsKlvNone :: M2tsKlv
pattern M2tsKlvNone = M2tsKlv' "NONE"

pattern M2tsKlvPassthrough :: M2tsKlv
pattern M2tsKlvPassthrough = M2tsKlv' "PASSTHROUGH"

{-# COMPLETE
  M2tsKlvNone,
  M2tsKlvPassthrough,
  M2tsKlv'
  #-}
