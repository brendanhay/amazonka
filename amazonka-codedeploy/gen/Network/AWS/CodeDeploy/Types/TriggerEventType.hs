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
-- Module      : Network.AWS.CodeDeploy.Types.TriggerEventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TriggerEventType
  ( TriggerEventType
      ( ..,
        TriggerEventType_DeploymentFailure,
        TriggerEventType_DeploymentReady,
        TriggerEventType_DeploymentRollback,
        TriggerEventType_DeploymentStart,
        TriggerEventType_DeploymentStop,
        TriggerEventType_DeploymentSuccess,
        TriggerEventType_InstanceFailure,
        TriggerEventType_InstanceReady,
        TriggerEventType_InstanceStart,
        TriggerEventType_InstanceSuccess
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TriggerEventType = TriggerEventType'
  { fromTriggerEventType ::
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

pattern TriggerEventType_DeploymentFailure :: TriggerEventType
pattern TriggerEventType_DeploymentFailure = TriggerEventType' "DeploymentFailure"

pattern TriggerEventType_DeploymentReady :: TriggerEventType
pattern TriggerEventType_DeploymentReady = TriggerEventType' "DeploymentReady"

pattern TriggerEventType_DeploymentRollback :: TriggerEventType
pattern TriggerEventType_DeploymentRollback = TriggerEventType' "DeploymentRollback"

pattern TriggerEventType_DeploymentStart :: TriggerEventType
pattern TriggerEventType_DeploymentStart = TriggerEventType' "DeploymentStart"

pattern TriggerEventType_DeploymentStop :: TriggerEventType
pattern TriggerEventType_DeploymentStop = TriggerEventType' "DeploymentStop"

pattern TriggerEventType_DeploymentSuccess :: TriggerEventType
pattern TriggerEventType_DeploymentSuccess = TriggerEventType' "DeploymentSuccess"

pattern TriggerEventType_InstanceFailure :: TriggerEventType
pattern TriggerEventType_InstanceFailure = TriggerEventType' "InstanceFailure"

pattern TriggerEventType_InstanceReady :: TriggerEventType
pattern TriggerEventType_InstanceReady = TriggerEventType' "InstanceReady"

pattern TriggerEventType_InstanceStart :: TriggerEventType
pattern TriggerEventType_InstanceStart = TriggerEventType' "InstanceStart"

pattern TriggerEventType_InstanceSuccess :: TriggerEventType
pattern TriggerEventType_InstanceSuccess = TriggerEventType' "InstanceSuccess"

{-# COMPLETE
  TriggerEventType_DeploymentFailure,
  TriggerEventType_DeploymentReady,
  TriggerEventType_DeploymentRollback,
  TriggerEventType_DeploymentStart,
  TriggerEventType_DeploymentStop,
  TriggerEventType_DeploymentSuccess,
  TriggerEventType_InstanceFailure,
  TriggerEventType_InstanceReady,
  TriggerEventType_InstanceStart,
  TriggerEventType_InstanceSuccess,
  TriggerEventType'
  #-}
