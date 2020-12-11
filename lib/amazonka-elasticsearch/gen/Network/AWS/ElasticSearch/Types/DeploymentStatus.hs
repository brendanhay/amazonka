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
        Completed,
        Eligible,
        InProgress,
        NotEligible,
        PendingUpdate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeploymentStatus = DeploymentStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Completed :: DeploymentStatus
pattern Completed = DeploymentStatus' "COMPLETED"

pattern Eligible :: DeploymentStatus
pattern Eligible = DeploymentStatus' "ELIGIBLE"

pattern InProgress :: DeploymentStatus
pattern InProgress = DeploymentStatus' "IN_PROGRESS"

pattern NotEligible :: DeploymentStatus
pattern NotEligible = DeploymentStatus' "NOT_ELIGIBLE"

pattern PendingUpdate :: DeploymentStatus
pattern PendingUpdate = DeploymentStatus' "PENDING_UPDATE"

{-# COMPLETE
  Completed,
  Eligible,
  InProgress,
  NotEligible,
  PendingUpdate,
  DeploymentStatus'
  #-}
