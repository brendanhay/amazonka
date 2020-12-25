{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsRateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsRateMode
  ( M2tsRateMode
      ( M2tsRateMode',
        M2tsRateModeCbr,
        M2tsRateModeVbr,
        fromM2tsRateMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M2ts Rate Mode
newtype M2tsRateMode = M2tsRateMode' {fromM2tsRateMode :: Core.Text}
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

pattern M2tsRateModeCbr :: M2tsRateMode
pattern M2tsRateModeCbr = M2tsRateMode' "CBR"

pattern M2tsRateModeVbr :: M2tsRateMode
pattern M2tsRateModeVbr = M2tsRateMode' "VBR"

{-# COMPLETE
  M2tsRateModeCbr,
  M2tsRateModeVbr,
  M2tsRateMode'
  #-}
