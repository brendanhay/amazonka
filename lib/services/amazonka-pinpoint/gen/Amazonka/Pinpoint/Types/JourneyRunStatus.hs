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
-- Module      : Amazonka.Pinpoint.Types.JourneyRunStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyRunStatus
  ( JourneyRunStatus
      ( ..,
        JourneyRunStatus_CANCELLED,
        JourneyRunStatus_COMPLETED,
        JourneyRunStatus_RUNNING,
        JourneyRunStatus_SCHEDULED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JourneyRunStatus = JourneyRunStatus'
  { fromJourneyRunStatus ::
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

pattern JourneyRunStatus_CANCELLED :: JourneyRunStatus
pattern JourneyRunStatus_CANCELLED = JourneyRunStatus' "CANCELLED"

pattern JourneyRunStatus_COMPLETED :: JourneyRunStatus
pattern JourneyRunStatus_COMPLETED = JourneyRunStatus' "COMPLETED"

pattern JourneyRunStatus_RUNNING :: JourneyRunStatus
pattern JourneyRunStatus_RUNNING = JourneyRunStatus' "RUNNING"

pattern JourneyRunStatus_SCHEDULED :: JourneyRunStatus
pattern JourneyRunStatus_SCHEDULED = JourneyRunStatus' "SCHEDULED"

{-# COMPLETE
  JourneyRunStatus_CANCELLED,
  JourneyRunStatus_COMPLETED,
  JourneyRunStatus_RUNNING,
  JourneyRunStatus_SCHEDULED,
  JourneyRunStatus'
  #-}
