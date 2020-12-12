{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
  ( Mpeg2RateControlMode
      ( Mpeg2RateControlMode',
        MRCMCbr,
        MRCMVbr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
newtype Mpeg2RateControlMode = Mpeg2RateControlMode' Lude.Text
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

pattern MRCMCbr :: Mpeg2RateControlMode
pattern MRCMCbr = Mpeg2RateControlMode' "CBR"

pattern MRCMVbr :: Mpeg2RateControlMode
pattern MRCMVbr = Mpeg2RateControlMode' "VBR"

{-# COMPLETE
  MRCMCbr,
  MRCMVbr,
  Mpeg2RateControlMode'
  #-}
