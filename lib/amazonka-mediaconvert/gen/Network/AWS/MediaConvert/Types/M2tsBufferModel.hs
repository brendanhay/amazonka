-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsBufferModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsBufferModel
  ( M2tsBufferModel
      ( M2tsBufferModel',
        MBMMultiplex,
        MBMNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
newtype M2tsBufferModel = M2tsBufferModel' Lude.Text
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

pattern MBMMultiplex :: M2tsBufferModel
pattern MBMMultiplex = M2tsBufferModel' "MULTIPLEX"

pattern MBMNone :: M2tsBufferModel
pattern MBMNone = M2tsBufferModel' "NONE"

{-# COMPLETE
  MBMMultiplex,
  MBMNone,
  M2tsBufferModel'
  #-}
