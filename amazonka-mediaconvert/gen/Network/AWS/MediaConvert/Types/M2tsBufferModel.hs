{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Controls what buffer model to use for accurate interleaving. If set to
-- MULTIPLEX, use multiplex buffer model. If set to NONE, this can lead to
-- lower latency, but low-memory devices may not be able to play back the
-- stream without interruptions.
newtype M2tsBufferModel = M2tsBufferModel'
  { fromM2tsBufferModel ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
