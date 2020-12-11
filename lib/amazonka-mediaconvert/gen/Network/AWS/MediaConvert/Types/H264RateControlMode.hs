-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264RateControlMode
  ( H264RateControlMode
      ( H264RateControlMode',
        HCbr,
        HQvbr,
        HVbr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
newtype H264RateControlMode = H264RateControlMode' Lude.Text
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

pattern HCbr :: H264RateControlMode
pattern HCbr = H264RateControlMode' "CBR"

pattern HQvbr :: H264RateControlMode
pattern HQvbr = H264RateControlMode' "QVBR"

pattern HVbr :: H264RateControlMode
pattern HVbr = H264RateControlMode' "VBR"

{-# COMPLETE
  HCbr,
  HQvbr,
  HVbr,
  H264RateControlMode'
  #-}
