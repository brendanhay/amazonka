-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264RateControlMode
  ( H264RateControlMode
      ( H264RateControlMode',
        HRCMCbr,
        HRCMMultiplex,
        HRCMQvbr,
        HRCMVbr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Rate Control Mode
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

pattern HRCMCbr :: H264RateControlMode
pattern HRCMCbr = H264RateControlMode' "CBR"

pattern HRCMMultiplex :: H264RateControlMode
pattern HRCMMultiplex = H264RateControlMode' "MULTIPLEX"

pattern HRCMQvbr :: H264RateControlMode
pattern HRCMQvbr = H264RateControlMode' "QVBR"

pattern HRCMVbr :: H264RateControlMode
pattern HRCMVbr = H264RateControlMode' "VBR"

{-# COMPLETE
  HRCMCbr,
  HRCMMultiplex,
  HRCMQvbr,
  HRCMVbr,
  H264RateControlMode'
  #-}
