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
        ASAuto,
        ASFixed,
        ASNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
newtype AfdSignaling = AfdSignaling' Lude.Text
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

pattern ASAuto :: AfdSignaling
pattern ASAuto = AfdSignaling' "AUTO"

pattern ASFixed :: AfdSignaling
pattern ASFixed = AfdSignaling' "FIXED"

pattern ASNone :: AfdSignaling
pattern ASNone = AfdSignaling' "NONE"

{-# COMPLETE
  ASAuto,
  ASFixed,
  ASNone,
  AfdSignaling'
  #-}
