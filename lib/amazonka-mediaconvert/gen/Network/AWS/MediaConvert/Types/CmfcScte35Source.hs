-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcScte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcScte35Source
  ( CmfcScte35Source
      ( CmfcScte35Source',
        CSSNone,
        CSSPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
newtype CmfcScte35Source = CmfcScte35Source' Lude.Text
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

pattern CSSNone :: CmfcScte35Source
pattern CSSNone = CmfcScte35Source' "NONE"

pattern CSSPassthrough :: CmfcScte35Source
pattern CSSPassthrough = CmfcScte35Source' "PASSTHROUGH"

{-# COMPLETE
  CSSNone,
  CSSPassthrough,
  CmfcScte35Source'
  #-}
