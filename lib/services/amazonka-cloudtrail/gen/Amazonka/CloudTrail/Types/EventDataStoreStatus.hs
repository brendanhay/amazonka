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
-- Module      : Amazonka.CloudTrail.Types.EventDataStoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.EventDataStoreStatus
  ( EventDataStoreStatus
      ( ..,
        EventDataStoreStatus_CREATED,
        EventDataStoreStatus_ENABLED,
        EventDataStoreStatus_PENDING_DELETION,
        EventDataStoreStatus_STARTING_INGESTION,
        EventDataStoreStatus_STOPPED_INGESTION,
        EventDataStoreStatus_STOPPING_INGESTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventDataStoreStatus = EventDataStoreStatus'
  { fromEventDataStoreStatus ::
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

pattern EventDataStoreStatus_CREATED :: EventDataStoreStatus
pattern EventDataStoreStatus_CREATED = EventDataStoreStatus' "CREATED"

pattern EventDataStoreStatus_ENABLED :: EventDataStoreStatus
pattern EventDataStoreStatus_ENABLED = EventDataStoreStatus' "ENABLED"

pattern EventDataStoreStatus_PENDING_DELETION :: EventDataStoreStatus
pattern EventDataStoreStatus_PENDING_DELETION = EventDataStoreStatus' "PENDING_DELETION"

pattern EventDataStoreStatus_STARTING_INGESTION :: EventDataStoreStatus
pattern EventDataStoreStatus_STARTING_INGESTION = EventDataStoreStatus' "STARTING_INGESTION"

pattern EventDataStoreStatus_STOPPED_INGESTION :: EventDataStoreStatus
pattern EventDataStoreStatus_STOPPED_INGESTION = EventDataStoreStatus' "STOPPED_INGESTION"

pattern EventDataStoreStatus_STOPPING_INGESTION :: EventDataStoreStatus
pattern EventDataStoreStatus_STOPPING_INGESTION = EventDataStoreStatus' "STOPPING_INGESTION"

{-# COMPLETE
  EventDataStoreStatus_CREATED,
  EventDataStoreStatus_ENABLED,
  EventDataStoreStatus_PENDING_DELETION,
  EventDataStoreStatus_STARTING_INGESTION,
  EventDataStoreStatus_STOPPED_INGESTION,
  EventDataStoreStatus_STOPPING_INGESTION,
  EventDataStoreStatus'
  #-}
