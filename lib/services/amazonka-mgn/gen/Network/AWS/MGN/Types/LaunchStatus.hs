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
-- Module      : Network.AWS.MGN.Types.LaunchStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.LaunchStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LaunchStatus = LaunchStatus'
  { fromLaunchStatus ::
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
