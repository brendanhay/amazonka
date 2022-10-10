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
-- Module      : Amazonka.FinSpaceData.Types.IngestionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.IngestionStatus
  ( IngestionStatus
      ( ..,
        IngestionStatus_FAILED,
        IngestionStatus_PENDING,
        IngestionStatus_RUNNING,
        IngestionStatus_STOP_REQUESTED,
        IngestionStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Status of the ingestion process returned from scheduler service.
newtype IngestionStatus = IngestionStatus'
  { fromIngestionStatus ::
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

pattern IngestionStatus_FAILED :: IngestionStatus
pattern IngestionStatus_FAILED = IngestionStatus' "FAILED"

pattern IngestionStatus_PENDING :: IngestionStatus
pattern IngestionStatus_PENDING = IngestionStatus' "PENDING"

pattern IngestionStatus_RUNNING :: IngestionStatus
pattern IngestionStatus_RUNNING = IngestionStatus' "RUNNING"

pattern IngestionStatus_STOP_REQUESTED :: IngestionStatus
pattern IngestionStatus_STOP_REQUESTED = IngestionStatus' "STOP_REQUESTED"

pattern IngestionStatus_SUCCESS :: IngestionStatus
pattern IngestionStatus_SUCCESS = IngestionStatus' "SUCCESS"

{-# COMPLETE
  IngestionStatus_FAILED,
  IngestionStatus_PENDING,
  IngestionStatus_RUNNING,
  IngestionStatus_STOP_REQUESTED,
  IngestionStatus_SUCCESS,
  IngestionStatus'
  #-}
