{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DeploymentStatus
  ( DeploymentStatus
      ( DeploymentStatus',
        DeploymentStatusPendingUpdate,
        DeploymentStatusInProgress,
        DeploymentStatusCompleted,
        DeploymentStatusNotEligible,
        DeploymentStatusEligible,
        fromDeploymentStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DeploymentStatusPendingUpdate :: DeploymentStatus
pattern DeploymentStatusPendingUpdate = DeploymentStatus' "PENDING_UPDATE"

pattern DeploymentStatusInProgress :: DeploymentStatus
pattern DeploymentStatusInProgress = DeploymentStatus' "IN_PROGRESS"

pattern DeploymentStatusCompleted :: DeploymentStatus
pattern DeploymentStatusCompleted = DeploymentStatus' "COMPLETED"

pattern DeploymentStatusNotEligible :: DeploymentStatus
pattern DeploymentStatusNotEligible = DeploymentStatus' "NOT_ELIGIBLE"

pattern DeploymentStatusEligible :: DeploymentStatus
pattern DeploymentStatusEligible = DeploymentStatus' "ELIGIBLE"

{-# COMPLETE
  DeploymentStatusPendingUpdate,
  DeploymentStatusInProgress,
  DeploymentStatusCompleted,
  DeploymentStatusNotEligible,
  DeploymentStatusEligible,
  DeploymentStatus'
  #-}
