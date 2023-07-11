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
-- Module      : Amazonka.DrS.Types.LaunchStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LaunchStatus
  ( LaunchStatus
      ( ..,
        LaunchStatus_FAILED,
        LaunchStatus_IN_PROGRESS,
        LaunchStatus_LAUNCHED,
        LaunchStatus_PENDING,
        LaunchStatus_TERMINATED
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

pattern LaunchStatus_FAILED :: LaunchStatus
pattern LaunchStatus_FAILED = LaunchStatus' "FAILED"

pattern LaunchStatus_IN_PROGRESS :: LaunchStatus
pattern LaunchStatus_IN_PROGRESS = LaunchStatus' "IN_PROGRESS"

pattern LaunchStatus_LAUNCHED :: LaunchStatus
pattern LaunchStatus_LAUNCHED = LaunchStatus' "LAUNCHED"

pattern LaunchStatus_PENDING :: LaunchStatus
pattern LaunchStatus_PENDING = LaunchStatus' "PENDING"

pattern LaunchStatus_TERMINATED :: LaunchStatus
pattern LaunchStatus_TERMINATED = LaunchStatus' "TERMINATED"

{-# COMPLETE
  LaunchStatus_FAILED,
  LaunchStatus_IN_PROGRESS,
  LaunchStatus_LAUNCHED,
  LaunchStatus_PENDING,
  LaunchStatus_TERMINATED,
  LaunchStatus'
  #-}
