{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsArib
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsArib
  ( M2tsArib
      ( M2tsArib',
        M2tsAribDisabled,
        M2tsAribEnabled,
        fromM2tsArib
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M2ts Arib
newtype M2tsArib = M2tsArib' {fromM2tsArib :: Core.Text}
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

pattern M2tsAribDisabled :: M2tsArib
pattern M2tsAribDisabled = M2tsArib' "DISABLED"

pattern M2tsAribEnabled :: M2tsArib
pattern M2tsAribEnabled = M2tsArib' "ENABLED"

{-# COMPLETE
  M2tsAribDisabled,
  M2tsAribEnabled,
  M2tsArib'
  #-}
