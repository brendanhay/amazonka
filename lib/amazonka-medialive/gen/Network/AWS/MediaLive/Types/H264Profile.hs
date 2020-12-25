{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Profile
  ( H264Profile
      ( H264Profile',
        H264ProfileBaseline,
        H264ProfileHigh,
        H264ProfileHigh10BIT,
        H264ProfileHigh422,
        H264ProfileHigh42210BIT,
        H264ProfileMain,
        fromH264Profile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Profile
newtype H264Profile = H264Profile' {fromH264Profile :: Core.Text}
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

pattern H264ProfileBaseline :: H264Profile
pattern H264ProfileBaseline = H264Profile' "BASELINE"

pattern H264ProfileHigh :: H264Profile
pattern H264ProfileHigh = H264Profile' "HIGH"

pattern H264ProfileHigh10BIT :: H264Profile
pattern H264ProfileHigh10BIT = H264Profile' "HIGH_10BIT"

pattern H264ProfileHigh422 :: H264Profile
pattern H264ProfileHigh422 = H264Profile' "HIGH_422"

pattern H264ProfileHigh42210BIT :: H264Profile
pattern H264ProfileHigh42210BIT = H264Profile' "HIGH_422_10BIT"

pattern H264ProfileMain :: H264Profile
pattern H264ProfileMain = H264Profile' "MAIN"

{-# COMPLETE
  H264ProfileBaseline,
  H264ProfileHigh,
  H264ProfileHigh10BIT,
  H264ProfileHigh422,
  H264ProfileHigh42210BIT,
  H264ProfileMain,
  H264Profile'
  #-}
