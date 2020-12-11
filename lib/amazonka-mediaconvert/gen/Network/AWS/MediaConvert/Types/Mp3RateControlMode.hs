-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp3RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3RateControlMode
  ( Mp3RateControlMode
      ( Mp3RateControlMode',
        MCbr,
        MVbr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
newtype Mp3RateControlMode = Mp3RateControlMode' Lude.Text
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

pattern MCbr :: Mp3RateControlMode
pattern MCbr = Mp3RateControlMode' "CBR"

pattern MVbr :: Mp3RateControlMode
pattern MVbr = Mp3RateControlMode' "VBR"

{-# COMPLETE
  MCbr,
  MVbr,
  Mp3RateControlMode'
  #-}
