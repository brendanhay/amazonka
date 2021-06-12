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
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentStatus
  ( BulkDeploymentStatus
      ( ..,
        BulkDeploymentStatus_Completed,
        BulkDeploymentStatus_Failed,
        BulkDeploymentStatus_Initializing,
        BulkDeploymentStatus_Running,
        BulkDeploymentStatus_Stopped,
        BulkDeploymentStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The current status of the bulk deployment.
newtype BulkDeploymentStatus = BulkDeploymentStatus'
  { fromBulkDeploymentStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern BulkDeploymentStatus_Completed :: BulkDeploymentStatus
pattern BulkDeploymentStatus_Completed = BulkDeploymentStatus' "Completed"

pattern BulkDeploymentStatus_Failed :: BulkDeploymentStatus
pattern BulkDeploymentStatus_Failed = BulkDeploymentStatus' "Failed"

pattern BulkDeploymentStatus_Initializing :: BulkDeploymentStatus
pattern BulkDeploymentStatus_Initializing = BulkDeploymentStatus' "Initializing"

pattern BulkDeploymentStatus_Running :: BulkDeploymentStatus
pattern BulkDeploymentStatus_Running = BulkDeploymentStatus' "Running"

pattern BulkDeploymentStatus_Stopped :: BulkDeploymentStatus
pattern BulkDeploymentStatus_Stopped = BulkDeploymentStatus' "Stopped"

pattern BulkDeploymentStatus_Stopping :: BulkDeploymentStatus
pattern BulkDeploymentStatus_Stopping = BulkDeploymentStatus' "Stopping"

{-# COMPLETE
  BulkDeploymentStatus_Completed,
  BulkDeploymentStatus_Failed,
  BulkDeploymentStatus_Initializing,
  BulkDeploymentStatus_Running,
  BulkDeploymentStatus_Stopped,
  BulkDeploymentStatus_Stopping,
  BulkDeploymentStatus'
  #-}
