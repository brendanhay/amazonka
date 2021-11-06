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
-- Module      : Amazonka.RobOMaker.Types.RobotStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RobotStatus
  ( RobotStatus
      ( ..,
        RobotStatus_Available,
        RobotStatus_Deploying,
        RobotStatus_Failed,
        RobotStatus_InSync,
        RobotStatus_NoResponse,
        RobotStatus_PendingNewDeployment,
        RobotStatus_Registered
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RobotStatus = RobotStatus'
  { fromRobotStatus ::
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

pattern RobotStatus_Available :: RobotStatus
pattern RobotStatus_Available = RobotStatus' "Available"

pattern RobotStatus_Deploying :: RobotStatus
pattern RobotStatus_Deploying = RobotStatus' "Deploying"

pattern RobotStatus_Failed :: RobotStatus
pattern RobotStatus_Failed = RobotStatus' "Failed"

pattern RobotStatus_InSync :: RobotStatus
pattern RobotStatus_InSync = RobotStatus' "InSync"

pattern RobotStatus_NoResponse :: RobotStatus
pattern RobotStatus_NoResponse = RobotStatus' "NoResponse"

pattern RobotStatus_PendingNewDeployment :: RobotStatus
pattern RobotStatus_PendingNewDeployment = RobotStatus' "PendingNewDeployment"

pattern RobotStatus_Registered :: RobotStatus
pattern RobotStatus_Registered = RobotStatus' "Registered"

{-# COMPLETE
  RobotStatus_Available,
  RobotStatus_Deploying,
  RobotStatus_Failed,
  RobotStatus_InSync,
  RobotStatus_NoResponse,
  RobotStatus_PendingNewDeployment,
  RobotStatus_Registered,
  RobotStatus'
  #-}
