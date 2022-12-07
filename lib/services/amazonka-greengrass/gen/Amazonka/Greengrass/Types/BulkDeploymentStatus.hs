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
-- Module      : Amazonka.Greengrass.Types.BulkDeploymentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.BulkDeploymentStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of the bulk deployment.
newtype BulkDeploymentStatus = BulkDeploymentStatus'
  { fromBulkDeploymentStatus ::
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
