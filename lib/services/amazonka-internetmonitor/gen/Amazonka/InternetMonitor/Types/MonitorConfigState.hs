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
-- Module      : Amazonka.InternetMonitor.Types.MonitorConfigState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.MonitorConfigState
  ( MonitorConfigState
      ( ..,
        MonitorConfigState_ACTIVE,
        MonitorConfigState_ERROR,
        MonitorConfigState_INACTIVE,
        MonitorConfigState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MonitorConfigState = MonitorConfigState'
  { fromMonitorConfigState ::
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

pattern MonitorConfigState_ACTIVE :: MonitorConfigState
pattern MonitorConfigState_ACTIVE = MonitorConfigState' "ACTIVE"

pattern MonitorConfigState_ERROR :: MonitorConfigState
pattern MonitorConfigState_ERROR = MonitorConfigState' "ERROR"

pattern MonitorConfigState_INACTIVE :: MonitorConfigState
pattern MonitorConfigState_INACTIVE = MonitorConfigState' "INACTIVE"

pattern MonitorConfigState_PENDING :: MonitorConfigState
pattern MonitorConfigState_PENDING = MonitorConfigState' "PENDING"

{-# COMPLETE
  MonitorConfigState_ACTIVE,
  MonitorConfigState_ERROR,
  MonitorConfigState_INACTIVE,
  MonitorConfigState_PENDING,
  MonitorConfigState'
  #-}
