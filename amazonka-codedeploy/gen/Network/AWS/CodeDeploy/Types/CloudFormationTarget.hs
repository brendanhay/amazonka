{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.CloudFormationTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.CloudFormationTarget where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the target to be updated by an AWS CloudFormation
-- blue\/green deployment. This target type is used for all deployments
-- initiated by a CloudFormation stack update.
--
-- /See:/ 'newCloudFormationTarget' smart constructor.
data CloudFormationTarget = CloudFormationTarget'
  { -- | The unique ID of an AWS CloudFormation blue\/green deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The status of an AWS CloudFormation blue\/green deployment\'s target
    -- application.
    status :: Prelude.Maybe TargetStatus,
    -- | The unique ID of a deployment target that has a type
    -- of @CloudFormationTarget@.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The percentage of production traffic that the target version of an AWS
    -- CloudFormation blue\/green deployment receives.
    targetVersionWeight :: Prelude.Maybe Prelude.Double,
    -- | The resource type for the AWS CloudFormation blue\/green deployment.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle events of the AWS CloudFormation blue\/green deployment to
    -- this target application.
    lifecycleEvents :: Prelude.Maybe [LifecycleEvent],
    -- | The date and time when the target application was updated by an AWS
    -- CloudFormation blue\/green deployment.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'cloudFormationTarget_deploymentId' - The unique ID of an AWS CloudFormation blue\/green deployment.
--
-- 'status', 'cloudFormationTarget_status' - The status of an AWS CloudFormation blue\/green deployment\'s target
-- application.
--
-- 'targetId', 'cloudFormationTarget_targetId' - The unique ID of a deployment target that has a type
-- of @CloudFormationTarget@.
--
-- 'targetVersionWeight', 'cloudFormationTarget_targetVersionWeight' - The percentage of production traffic that the target version of an AWS
-- CloudFormation blue\/green deployment receives.
--
-- 'resourceType', 'cloudFormationTarget_resourceType' - The resource type for the AWS CloudFormation blue\/green deployment.
--
-- 'lifecycleEvents', 'cloudFormationTarget_lifecycleEvents' - The lifecycle events of the AWS CloudFormation blue\/green deployment to
-- this target application.
--
-- 'lastUpdatedAt', 'cloudFormationTarget_lastUpdatedAt' - The date and time when the target application was updated by an AWS
-- CloudFormation blue\/green deployment.
newCloudFormationTarget ::
  CloudFormationTarget
newCloudFormationTarget =
  CloudFormationTarget'
    { deploymentId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      targetId = Prelude.Nothing,
      targetVersionWeight = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      lifecycleEvents = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing
    }

-- | The unique ID of an AWS CloudFormation blue\/green deployment.
cloudFormationTarget_deploymentId :: Lens.Lens' CloudFormationTarget (Prelude.Maybe Prelude.Text)
cloudFormationTarget_deploymentId = Lens.lens (\CloudFormationTarget' {deploymentId} -> deploymentId) (\s@CloudFormationTarget' {} a -> s {deploymentId = a} :: CloudFormationTarget)

-- | The status of an AWS CloudFormation blue\/green deployment\'s target
-- application.
cloudFormationTarget_status :: Lens.Lens' CloudFormationTarget (Prelude.Maybe TargetStatus)
cloudFormationTarget_status = Lens.lens (\CloudFormationTarget' {status} -> status) (\s@CloudFormationTarget' {} a -> s {status = a} :: CloudFormationTarget)

-- | The unique ID of a deployment target that has a type
-- of @CloudFormationTarget@.
cloudFormationTarget_targetId :: Lens.Lens' CloudFormationTarget (Prelude.Maybe Prelude.Text)
cloudFormationTarget_targetId = Lens.lens (\CloudFormationTarget' {targetId} -> targetId) (\s@CloudFormationTarget' {} a -> s {targetId = a} :: CloudFormationTarget)

-- | The percentage of production traffic that the target version of an AWS
-- CloudFormation blue\/green deployment receives.
cloudFormationTarget_targetVersionWeight :: Lens.Lens' CloudFormationTarget (Prelude.Maybe Prelude.Double)
cloudFormationTarget_targetVersionWeight = Lens.lens (\CloudFormationTarget' {targetVersionWeight} -> targetVersionWeight) (\s@CloudFormationTarget' {} a -> s {targetVersionWeight = a} :: CloudFormationTarget)

-- | The resource type for the AWS CloudFormation blue\/green deployment.
cloudFormationTarget_resourceType :: Lens.Lens' CloudFormationTarget (Prelude.Maybe Prelude.Text)
cloudFormationTarget_resourceType = Lens.lens (\CloudFormationTarget' {resourceType} -> resourceType) (\s@CloudFormationTarget' {} a -> s {resourceType = a} :: CloudFormationTarget)

-- | The lifecycle events of the AWS CloudFormation blue\/green deployment to
-- this target application.
cloudFormationTarget_lifecycleEvents :: Lens.Lens' CloudFormationTarget (Prelude.Maybe [LifecycleEvent])
cloudFormationTarget_lifecycleEvents = Lens.lens (\CloudFormationTarget' {lifecycleEvents} -> lifecycleEvents) (\s@CloudFormationTarget' {} a -> s {lifecycleEvents = a} :: CloudFormationTarget) Prelude.. Lens.mapping Prelude._Coerce

-- | The date and time when the target application was updated by an AWS
-- CloudFormation blue\/green deployment.
cloudFormationTarget_lastUpdatedAt :: Lens.Lens' CloudFormationTarget (Prelude.Maybe Prelude.UTCTime)
cloudFormationTarget_lastUpdatedAt = Lens.lens (\CloudFormationTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@CloudFormationTarget' {} a -> s {lastUpdatedAt = a} :: CloudFormationTarget) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON CloudFormationTarget where
  parseJSON =
    Prelude.withObject
      "CloudFormationTarget"
      ( \x ->
          CloudFormationTarget'
            Prelude.<$> (x Prelude..:? "deploymentId")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "targetId")
            Prelude.<*> (x Prelude..:? "targetVersionWeight")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> ( x Prelude..:? "lifecycleEvents"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "lastUpdatedAt")
      )

instance Prelude.Hashable CloudFormationTarget

instance Prelude.NFData CloudFormationTarget
