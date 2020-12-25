{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Duration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Duration
  ( Duration
      ( Duration',
        DurationHr24,
        DurationDay7,
        DurationDay14,
        DurationDay30,
        fromDuration
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Duration = Duration' {fromDuration :: Core.Text}
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

pattern DurationHr24 :: Duration
pattern DurationHr24 = Duration' "HR_24"

pattern DurationDay7 :: Duration
pattern DurationDay7 = Duration' "DAY_7"

pattern DurationDay14 :: Duration
pattern DurationDay14 = Duration' "DAY_14"

pattern DurationDay30 :: Duration
pattern DurationDay30 = Duration' "DAY_30"

{-# COMPLETE
  DurationHr24,
  DurationDay7,
  DurationDay14,
  DurationDay30,
  Duration'
  #-}
