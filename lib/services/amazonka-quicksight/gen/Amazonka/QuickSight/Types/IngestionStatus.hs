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
-- Module      : Amazonka.QuickSight.Types.IngestionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IngestionStatus
  ( IngestionStatus
      ( ..,
        IngestionStatus_CANCELLED,
        IngestionStatus_COMPLETED,
        IngestionStatus_FAILED,
        IngestionStatus_INITIALIZED,
        IngestionStatus_QUEUED,
        IngestionStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IngestionStatus = IngestionStatus'
  { fromIngestionStatus ::
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

pattern IngestionStatus_CANCELLED :: IngestionStatus
pattern IngestionStatus_CANCELLED = IngestionStatus' "CANCELLED"

pattern IngestionStatus_COMPLETED :: IngestionStatus
pattern IngestionStatus_COMPLETED = IngestionStatus' "COMPLETED"

pattern IngestionStatus_FAILED :: IngestionStatus
pattern IngestionStatus_FAILED = IngestionStatus' "FAILED"

pattern IngestionStatus_INITIALIZED :: IngestionStatus
pattern IngestionStatus_INITIALIZED = IngestionStatus' "INITIALIZED"

pattern IngestionStatus_QUEUED :: IngestionStatus
pattern IngestionStatus_QUEUED = IngestionStatus' "QUEUED"

pattern IngestionStatus_RUNNING :: IngestionStatus
pattern IngestionStatus_RUNNING = IngestionStatus' "RUNNING"

{-# COMPLETE
  IngestionStatus_CANCELLED,
  IngestionStatus_COMPLETED,
  IngestionStatus_FAILED,
  IngestionStatus_INITIALIZED,
  IngestionStatus_QUEUED,
  IngestionStatus_RUNNING,
  IngestionStatus'
  #-}
