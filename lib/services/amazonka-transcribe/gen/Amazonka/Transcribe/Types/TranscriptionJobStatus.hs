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
-- Module      : Amazonka.Transcribe.Types.TranscriptionJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.TranscriptionJobStatus
  ( TranscriptionJobStatus
      ( ..,
        TranscriptionJobStatus_COMPLETED,
        TranscriptionJobStatus_FAILED,
        TranscriptionJobStatus_IN_PROGRESS,
        TranscriptionJobStatus_QUEUED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscriptionJobStatus = TranscriptionJobStatus'
  { fromTranscriptionJobStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern TranscriptionJobStatus_COMPLETED :: TranscriptionJobStatus
pattern TranscriptionJobStatus_COMPLETED = TranscriptionJobStatus' "COMPLETED"

pattern TranscriptionJobStatus_FAILED :: TranscriptionJobStatus
pattern TranscriptionJobStatus_FAILED = TranscriptionJobStatus' "FAILED"

pattern TranscriptionJobStatus_IN_PROGRESS :: TranscriptionJobStatus
pattern TranscriptionJobStatus_IN_PROGRESS = TranscriptionJobStatus' "IN_PROGRESS"

pattern TranscriptionJobStatus_QUEUED :: TranscriptionJobStatus
pattern TranscriptionJobStatus_QUEUED = TranscriptionJobStatus' "QUEUED"

{-# COMPLETE
  TranscriptionJobStatus_COMPLETED,
  TranscriptionJobStatus_FAILED,
  TranscriptionJobStatus_IN_PROGRESS,
  TranscriptionJobStatus_QUEUED,
  TranscriptionJobStatus'
  #-}
