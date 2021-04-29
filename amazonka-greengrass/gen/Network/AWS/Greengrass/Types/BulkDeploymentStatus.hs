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

import qualified Network.AWS.Prelude as Prelude

-- | The current status of the bulk deployment.
newtype BulkDeploymentStatus = BulkDeploymentStatus'
  { fromBulkDeploymentStatus ::
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
