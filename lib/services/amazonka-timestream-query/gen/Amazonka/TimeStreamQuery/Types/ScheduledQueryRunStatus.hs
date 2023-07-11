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
-- Module      : Amazonka.TimeStreamQuery.Types.ScheduledQueryRunStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ScheduledQueryRunStatus
  ( ScheduledQueryRunStatus
      ( ..,
        ScheduledQueryRunStatus_AUTO_TRIGGER_FAILURE,
        ScheduledQueryRunStatus_AUTO_TRIGGER_SUCCESS,
        ScheduledQueryRunStatus_MANUAL_TRIGGER_FAILURE,
        ScheduledQueryRunStatus_MANUAL_TRIGGER_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduledQueryRunStatus = ScheduledQueryRunStatus'
  { fromScheduledQueryRunStatus ::
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

pattern ScheduledQueryRunStatus_AUTO_TRIGGER_FAILURE :: ScheduledQueryRunStatus
pattern ScheduledQueryRunStatus_AUTO_TRIGGER_FAILURE = ScheduledQueryRunStatus' "AUTO_TRIGGER_FAILURE"

pattern ScheduledQueryRunStatus_AUTO_TRIGGER_SUCCESS :: ScheduledQueryRunStatus
pattern ScheduledQueryRunStatus_AUTO_TRIGGER_SUCCESS = ScheduledQueryRunStatus' "AUTO_TRIGGER_SUCCESS"

pattern ScheduledQueryRunStatus_MANUAL_TRIGGER_FAILURE :: ScheduledQueryRunStatus
pattern ScheduledQueryRunStatus_MANUAL_TRIGGER_FAILURE = ScheduledQueryRunStatus' "MANUAL_TRIGGER_FAILURE"

pattern ScheduledQueryRunStatus_MANUAL_TRIGGER_SUCCESS :: ScheduledQueryRunStatus
pattern ScheduledQueryRunStatus_MANUAL_TRIGGER_SUCCESS = ScheduledQueryRunStatus' "MANUAL_TRIGGER_SUCCESS"

{-# COMPLETE
  ScheduledQueryRunStatus_AUTO_TRIGGER_FAILURE,
  ScheduledQueryRunStatus_AUTO_TRIGGER_SUCCESS,
  ScheduledQueryRunStatus_MANUAL_TRIGGER_FAILURE,
  ScheduledQueryRunStatus_MANUAL_TRIGGER_SUCCESS,
  ScheduledQueryRunStatus'
  #-}
