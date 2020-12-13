{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentStatus
  ( DeploymentStatus
      ( DeploymentStatus',
        Created,
        Queued,
        InProgress,
        Baking,
        Succeeded,
        Failed,
        Stopped,
        Ready
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

pattern Created :: DeploymentStatus
pattern Created = DeploymentStatus' "Created"

pattern Queued :: DeploymentStatus
pattern Queued = DeploymentStatus' "Queued"

pattern InProgress :: DeploymentStatus
pattern InProgress = DeploymentStatus' "InProgress"

pattern Baking :: DeploymentStatus
pattern Baking = DeploymentStatus' "Baking"

pattern Succeeded :: DeploymentStatus
pattern Succeeded = DeploymentStatus' "Succeeded"

pattern Failed :: DeploymentStatus
pattern Failed = DeploymentStatus' "Failed"

pattern Stopped :: DeploymentStatus
pattern Stopped = DeploymentStatus' "Stopped"

pattern Ready :: DeploymentStatus
pattern Ready = DeploymentStatus' "Ready"

{-# COMPLETE
  Created,
  Queued,
  InProgress,
  Baking,
  Succeeded,
  Failed,
  Stopped,
  Ready,
  DeploymentStatus'
  #-}
