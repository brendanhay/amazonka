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
-- Module      : Network.AWS.Transcribe.Types.ModelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_COMPLETED,
        ModelStatus_FAILED,
        ModelStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ModelStatus = ModelStatus'
  { fromModelStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ModelStatus_COMPLETED :: ModelStatus
pattern ModelStatus_COMPLETED = ModelStatus' "COMPLETED"

pattern ModelStatus_FAILED :: ModelStatus
pattern ModelStatus_FAILED = ModelStatus' "FAILED"

pattern ModelStatus_IN_PROGRESS :: ModelStatus
pattern ModelStatus_IN_PROGRESS = ModelStatus' "IN_PROGRESS"

{-# COMPLETE
  ModelStatus_COMPLETED,
  ModelStatus_FAILED,
  ModelStatus_IN_PROGRESS,
  ModelStatus'
  #-}
