{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        M2tsBufferModelMultiplex,
        M2tsBufferModelNone,
        fromM2tsBufferModel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
newtype M2tsBufferModel = M2tsBufferModel'
  { fromM2tsBufferModel ::
      Core.Text
  }
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

pattern M2tsBufferModelMultiplex :: M2tsBufferModel
pattern M2tsBufferModelMultiplex = M2tsBufferModel' "MULTIPLEX"

pattern M2tsBufferModelNone :: M2tsBufferModel
pattern M2tsBufferModelNone = M2tsBufferModel' "NONE"

{-# COMPLETE
  M2tsBufferModelMultiplex,
  M2tsBufferModelNone,
  M2tsBufferModel'
  #-}
