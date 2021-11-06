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
-- Module      : Amazonka.RobOMaker.Types.RobotDeploymentStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RobotDeploymentStep
  ( RobotDeploymentStep
      ( ..,
        RobotDeploymentStep_DownloadingExtracting,
        RobotDeploymentStep_ExecutingDownloadCondition,
        RobotDeploymentStep_ExecutingPostLaunch,
        RobotDeploymentStep_ExecutingPreLaunch,
        RobotDeploymentStep_Finished,
        RobotDeploymentStep_Launching,
        RobotDeploymentStep_Validating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RobotDeploymentStep = RobotDeploymentStep'
  { fromRobotDeploymentStep ::
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

pattern RobotDeploymentStep_DownloadingExtracting :: RobotDeploymentStep
pattern RobotDeploymentStep_DownloadingExtracting = RobotDeploymentStep' "DownloadingExtracting"

pattern RobotDeploymentStep_ExecutingDownloadCondition :: RobotDeploymentStep
pattern RobotDeploymentStep_ExecutingDownloadCondition = RobotDeploymentStep' "ExecutingDownloadCondition"

pattern RobotDeploymentStep_ExecutingPostLaunch :: RobotDeploymentStep
pattern RobotDeploymentStep_ExecutingPostLaunch = RobotDeploymentStep' "ExecutingPostLaunch"

pattern RobotDeploymentStep_ExecutingPreLaunch :: RobotDeploymentStep
pattern RobotDeploymentStep_ExecutingPreLaunch = RobotDeploymentStep' "ExecutingPreLaunch"

pattern RobotDeploymentStep_Finished :: RobotDeploymentStep
pattern RobotDeploymentStep_Finished = RobotDeploymentStep' "Finished"

pattern RobotDeploymentStep_Launching :: RobotDeploymentStep
pattern RobotDeploymentStep_Launching = RobotDeploymentStep' "Launching"

pattern RobotDeploymentStep_Validating :: RobotDeploymentStep
pattern RobotDeploymentStep_Validating = RobotDeploymentStep' "Validating"

{-# COMPLETE
  RobotDeploymentStep_DownloadingExtracting,
  RobotDeploymentStep_ExecutingDownloadCondition,
  RobotDeploymentStep_ExecutingPostLaunch,
  RobotDeploymentStep_ExecutingPreLaunch,
  RobotDeploymentStep_Finished,
  RobotDeploymentStep_Launching,
  RobotDeploymentStep_Validating,
  RobotDeploymentStep'
  #-}
