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
-- Module      : Amazonka.Evidently.Types.LaunchStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.LaunchStatus
  ( LaunchStatus
      ( ..,
        LaunchStatus_CANCELLED,
        LaunchStatus_COMPLETED,
        LaunchStatus_CREATED,
        LaunchStatus_RUNNING,
        LaunchStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchStatus = LaunchStatus'
  { fromLaunchStatus ::
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

pattern LaunchStatus_CANCELLED :: LaunchStatus
pattern LaunchStatus_CANCELLED = LaunchStatus' "CANCELLED"

pattern LaunchStatus_COMPLETED :: LaunchStatus
pattern LaunchStatus_COMPLETED = LaunchStatus' "COMPLETED"

pattern LaunchStatus_CREATED :: LaunchStatus
pattern LaunchStatus_CREATED = LaunchStatus' "CREATED"

pattern LaunchStatus_RUNNING :: LaunchStatus
pattern LaunchStatus_RUNNING = LaunchStatus' "RUNNING"

pattern LaunchStatus_UPDATING :: LaunchStatus
pattern LaunchStatus_UPDATING = LaunchStatus' "UPDATING"

{-# COMPLETE
  LaunchStatus_CANCELLED,
  LaunchStatus_COMPLETED,
  LaunchStatus_CREATED,
  LaunchStatus_RUNNING,
  LaunchStatus_UPDATING,
  LaunchStatus'
  #-}
