{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsBufferModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsBufferModel
  ( M2tsBufferModel
      ( ..,
        M2tsBufferModel_MULTIPLEX,
        M2tsBufferModel_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Controls what buffer model to use for accurate interleaving. If set to
-- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
-- lower latency, but low-memory devices may not be able to play back the
-- stream without interruptions.
newtype M2tsBufferModel = M2tsBufferModel'
  { fromM2tsBufferModel ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern M2tsBufferModel_MULTIPLEX :: M2tsBufferModel
pattern M2tsBufferModel_MULTIPLEX = M2tsBufferModel' "MULTIPLEX"

pattern M2tsBufferModel_NONE :: M2tsBufferModel
pattern M2tsBufferModel_NONE = M2tsBufferModel' "NONE"

{-# COMPLETE
  M2tsBufferModel_MULTIPLEX,
  M2tsBufferModel_NONE,
  M2tsBufferModel'
  #-}
