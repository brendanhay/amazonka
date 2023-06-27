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
-- Module      : Amazonka.Comprehend.Types.FlywheelStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelStatus
  ( FlywheelStatus
      ( ..,
        FlywheelStatus_ACTIVE,
        FlywheelStatus_CREATING,
        FlywheelStatus_DELETING,
        FlywheelStatus_FAILED,
        FlywheelStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FlywheelStatus = FlywheelStatus'
  { fromFlywheelStatus ::
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

pattern FlywheelStatus_ACTIVE :: FlywheelStatus
pattern FlywheelStatus_ACTIVE = FlywheelStatus' "ACTIVE"

pattern FlywheelStatus_CREATING :: FlywheelStatus
pattern FlywheelStatus_CREATING = FlywheelStatus' "CREATING"

pattern FlywheelStatus_DELETING :: FlywheelStatus
pattern FlywheelStatus_DELETING = FlywheelStatus' "DELETING"

pattern FlywheelStatus_FAILED :: FlywheelStatus
pattern FlywheelStatus_FAILED = FlywheelStatus' "FAILED"

pattern FlywheelStatus_UPDATING :: FlywheelStatus
pattern FlywheelStatus_UPDATING = FlywheelStatus' "UPDATING"

{-# COMPLETE
  FlywheelStatus_ACTIVE,
  FlywheelStatus_CREATING,
  FlywheelStatus_DELETING,
  FlywheelStatus_FAILED,
  FlywheelStatus_UPDATING,
  FlywheelStatus'
  #-}
