{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AfdSignaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AfdSignaling
  ( AfdSignaling
      ( AfdSignaling',
        AfdSignalingNone,
        AfdSignalingAuto,
        AfdSignalingFixed,
        fromAfdSignaling
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
newtype AfdSignaling = AfdSignaling' {fromAfdSignaling :: Core.Text}
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

pattern AfdSignalingNone :: AfdSignaling
pattern AfdSignalingNone = AfdSignaling' "NONE"

pattern AfdSignalingAuto :: AfdSignaling
pattern AfdSignalingAuto = AfdSignaling' "AUTO"

pattern AfdSignalingFixed :: AfdSignaling
pattern AfdSignalingFixed = AfdSignaling' "FIXED"

{-# COMPLETE
  AfdSignalingNone,
  AfdSignalingAuto,
  AfdSignalingFixed,
  AfdSignaling'
  #-}
