{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentRolloutState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentRolloutState
  ( DeploymentRolloutState
      ( ..,
        DeploymentRolloutState_COMPLETED,
        DeploymentRolloutState_FAILED,
        DeploymentRolloutState_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeploymentRolloutState = DeploymentRolloutState'
  { fromDeploymentRolloutState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DeploymentRolloutState_COMPLETED :: DeploymentRolloutState
pattern DeploymentRolloutState_COMPLETED = DeploymentRolloutState' "COMPLETED"

pattern DeploymentRolloutState_FAILED :: DeploymentRolloutState
pattern DeploymentRolloutState_FAILED = DeploymentRolloutState' "FAILED"

pattern DeploymentRolloutState_IN_PROGRESS :: DeploymentRolloutState
pattern DeploymentRolloutState_IN_PROGRESS = DeploymentRolloutState' "IN_PROGRESS"

{-# COMPLETE
  DeploymentRolloutState_COMPLETED,
  DeploymentRolloutState_FAILED,
  DeploymentRolloutState_IN_PROGRESS,
  DeploymentRolloutState'
  #-}
