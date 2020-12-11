-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265RateControlMode
  ( H265RateControlMode
      ( H265RateControlMode',
        HRCMCbr,
        HRCMQvbr,
        HRCMVbr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
newtype H265RateControlMode = H265RateControlMode' Lude.Text
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

pattern HRCMCbr :: H265RateControlMode
pattern HRCMCbr = H265RateControlMode' "CBR"

pattern HRCMQvbr :: H265RateControlMode
pattern HRCMQvbr = H265RateControlMode' "QVBR"

pattern HRCMVbr :: H265RateControlMode
pattern HRCMVbr = H265RateControlMode' "VBR"

{-# COMPLETE
  HRCMCbr,
  HRCMQvbr,
  HRCMVbr,
  H265RateControlMode'
  #-}
