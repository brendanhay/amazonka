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
-- Module      : Amazonka.Connect.Types.HistoricalMetricName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HistoricalMetricName
  ( HistoricalMetricName
      ( ..,
        HistoricalMetricName_ABANDON_TIME,
        HistoricalMetricName_AFTER_CONTACT_WORK_TIME,
        HistoricalMetricName_API_CONTACTS_HANDLED,
        HistoricalMetricName_CALLBACK_CONTACTS_HANDLED,
        HistoricalMetricName_CONTACTS_ABANDONED,
        HistoricalMetricName_CONTACTS_AGENT_HUNG_UP_FIRST,
        HistoricalMetricName_CONTACTS_CONSULTED,
        HistoricalMetricName_CONTACTS_HANDLED,
        HistoricalMetricName_CONTACTS_HANDLED_INCOMING,
        HistoricalMetricName_CONTACTS_HANDLED_OUTBOUND,
        HistoricalMetricName_CONTACTS_HOLD_ABANDONS,
        HistoricalMetricName_CONTACTS_MISSED,
        HistoricalMetricName_CONTACTS_QUEUED,
        HistoricalMetricName_CONTACTS_TRANSFERRED_IN,
        HistoricalMetricName_CONTACTS_TRANSFERRED_IN_FROM_QUEUE,
        HistoricalMetricName_CONTACTS_TRANSFERRED_OUT,
        HistoricalMetricName_CONTACTS_TRANSFERRED_OUT_FROM_QUEUE,
        HistoricalMetricName_HANDLE_TIME,
        HistoricalMetricName_HOLD_TIME,
        HistoricalMetricName_INTERACTION_AND_HOLD_TIME,
        HistoricalMetricName_INTERACTION_TIME,
        HistoricalMetricName_OCCUPANCY,
        HistoricalMetricName_QUEUED_TIME,
        HistoricalMetricName_QUEUE_ANSWER_TIME,
        HistoricalMetricName_SERVICE_LEVEL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The historical metric names.
newtype HistoricalMetricName = HistoricalMetricName'
  { fromHistoricalMetricName ::
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

pattern HistoricalMetricName_ABANDON_TIME :: HistoricalMetricName
pattern HistoricalMetricName_ABANDON_TIME = HistoricalMetricName' "ABANDON_TIME"

pattern HistoricalMetricName_AFTER_CONTACT_WORK_TIME :: HistoricalMetricName
pattern HistoricalMetricName_AFTER_CONTACT_WORK_TIME = HistoricalMetricName' "AFTER_CONTACT_WORK_TIME"

pattern HistoricalMetricName_API_CONTACTS_HANDLED :: HistoricalMetricName
pattern HistoricalMetricName_API_CONTACTS_HANDLED = HistoricalMetricName' "API_CONTACTS_HANDLED"

pattern HistoricalMetricName_CALLBACK_CONTACTS_HANDLED :: HistoricalMetricName
pattern HistoricalMetricName_CALLBACK_CONTACTS_HANDLED = HistoricalMetricName' "CALLBACK_CONTACTS_HANDLED"

pattern HistoricalMetricName_CONTACTS_ABANDONED :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_ABANDONED = HistoricalMetricName' "CONTACTS_ABANDONED"

pattern HistoricalMetricName_CONTACTS_AGENT_HUNG_UP_FIRST :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_AGENT_HUNG_UP_FIRST = HistoricalMetricName' "CONTACTS_AGENT_HUNG_UP_FIRST"

pattern HistoricalMetricName_CONTACTS_CONSULTED :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_CONSULTED = HistoricalMetricName' "CONTACTS_CONSULTED"

pattern HistoricalMetricName_CONTACTS_HANDLED :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_HANDLED = HistoricalMetricName' "CONTACTS_HANDLED"

pattern HistoricalMetricName_CONTACTS_HANDLED_INCOMING :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_HANDLED_INCOMING = HistoricalMetricName' "CONTACTS_HANDLED_INCOMING"

pattern HistoricalMetricName_CONTACTS_HANDLED_OUTBOUND :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_HANDLED_OUTBOUND = HistoricalMetricName' "CONTACTS_HANDLED_OUTBOUND"

pattern HistoricalMetricName_CONTACTS_HOLD_ABANDONS :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_HOLD_ABANDONS = HistoricalMetricName' "CONTACTS_HOLD_ABANDONS"

pattern HistoricalMetricName_CONTACTS_MISSED :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_MISSED = HistoricalMetricName' "CONTACTS_MISSED"

pattern HistoricalMetricName_CONTACTS_QUEUED :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_QUEUED = HistoricalMetricName' "CONTACTS_QUEUED"

pattern HistoricalMetricName_CONTACTS_TRANSFERRED_IN :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_TRANSFERRED_IN = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN"

pattern HistoricalMetricName_CONTACTS_TRANSFERRED_IN_FROM_QUEUE :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_TRANSFERRED_IN_FROM_QUEUE = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN_FROM_QUEUE"

pattern HistoricalMetricName_CONTACTS_TRANSFERRED_OUT :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_TRANSFERRED_OUT = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT"

pattern HistoricalMetricName_CONTACTS_TRANSFERRED_OUT_FROM_QUEUE :: HistoricalMetricName
pattern HistoricalMetricName_CONTACTS_TRANSFERRED_OUT_FROM_QUEUE = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT_FROM_QUEUE"

pattern HistoricalMetricName_HANDLE_TIME :: HistoricalMetricName
pattern HistoricalMetricName_HANDLE_TIME = HistoricalMetricName' "HANDLE_TIME"

pattern HistoricalMetricName_HOLD_TIME :: HistoricalMetricName
pattern HistoricalMetricName_HOLD_TIME = HistoricalMetricName' "HOLD_TIME"

pattern HistoricalMetricName_INTERACTION_AND_HOLD_TIME :: HistoricalMetricName
pattern HistoricalMetricName_INTERACTION_AND_HOLD_TIME = HistoricalMetricName' "INTERACTION_AND_HOLD_TIME"

pattern HistoricalMetricName_INTERACTION_TIME :: HistoricalMetricName
pattern HistoricalMetricName_INTERACTION_TIME = HistoricalMetricName' "INTERACTION_TIME"

pattern HistoricalMetricName_OCCUPANCY :: HistoricalMetricName
pattern HistoricalMetricName_OCCUPANCY = HistoricalMetricName' "OCCUPANCY"

pattern HistoricalMetricName_QUEUED_TIME :: HistoricalMetricName
pattern HistoricalMetricName_QUEUED_TIME = HistoricalMetricName' "QUEUED_TIME"

pattern HistoricalMetricName_QUEUE_ANSWER_TIME :: HistoricalMetricName
pattern HistoricalMetricName_QUEUE_ANSWER_TIME = HistoricalMetricName' "QUEUE_ANSWER_TIME"

pattern HistoricalMetricName_SERVICE_LEVEL :: HistoricalMetricName
pattern HistoricalMetricName_SERVICE_LEVEL = HistoricalMetricName' "SERVICE_LEVEL"

{-# COMPLETE
  HistoricalMetricName_ABANDON_TIME,
  HistoricalMetricName_AFTER_CONTACT_WORK_TIME,
  HistoricalMetricName_API_CONTACTS_HANDLED,
  HistoricalMetricName_CALLBACK_CONTACTS_HANDLED,
  HistoricalMetricName_CONTACTS_ABANDONED,
  HistoricalMetricName_CONTACTS_AGENT_HUNG_UP_FIRST,
  HistoricalMetricName_CONTACTS_CONSULTED,
  HistoricalMetricName_CONTACTS_HANDLED,
  HistoricalMetricName_CONTACTS_HANDLED_INCOMING,
  HistoricalMetricName_CONTACTS_HANDLED_OUTBOUND,
  HistoricalMetricName_CONTACTS_HOLD_ABANDONS,
  HistoricalMetricName_CONTACTS_MISSED,
  HistoricalMetricName_CONTACTS_QUEUED,
  HistoricalMetricName_CONTACTS_TRANSFERRED_IN,
  HistoricalMetricName_CONTACTS_TRANSFERRED_IN_FROM_QUEUE,
  HistoricalMetricName_CONTACTS_TRANSFERRED_OUT,
  HistoricalMetricName_CONTACTS_TRANSFERRED_OUT_FROM_QUEUE,
  HistoricalMetricName_HANDLE_TIME,
  HistoricalMetricName_HOLD_TIME,
  HistoricalMetricName_INTERACTION_AND_HOLD_TIME,
  HistoricalMetricName_INTERACTION_TIME,
  HistoricalMetricName_OCCUPANCY,
  HistoricalMetricName_QUEUED_TIME,
  HistoricalMetricName_QUEUE_ANSWER_TIME,
  HistoricalMetricName_SERVICE_LEVEL,
  HistoricalMetricName'
  #-}
