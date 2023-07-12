{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.AttachInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more EC2 instances to the specified Auto Scaling group.
--
-- When you attach instances, Amazon EC2 Auto Scaling increases the desired
-- capacity of the group by the number of instances being attached. If the
-- number of instances being attached plus the desired capacity of the
-- group exceeds the maximum size of the group, the operation fails.
--
-- If there is a Classic Load Balancer attached to your Auto Scaling group,
-- the instances are also registered with the load balancer. If there are
-- target groups attached to your Auto Scaling group, the instances are
-- also registered with the target groups.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/attach-instance-asg.html Attach EC2 instances to your Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.AttachInstances
  ( -- * Creating a Request
    AttachInstances (..),
    newAttachInstances,

    -- * Request Lenses
    attachInstances_instanceIds,
    attachInstances_autoScalingGroupName,

    -- * Destructuring the Response
    AttachInstancesResponse (..),
    newAttachInstancesResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachInstances' smart constructor.
data AttachInstances = AttachInstances'
  { -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'attachInstances_instanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- 'autoScalingGroupName', 'attachInstances_autoScalingGroupName' - The name of the Auto Scaling group.
newAttachInstances ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  AttachInstances
newAttachInstances pAutoScalingGroupName_ =
  AttachInstances'
    { instanceIds = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The IDs of the instances. You can specify up to 20 instances.
attachInstances_instanceIds :: Lens.Lens' AttachInstances (Prelude.Maybe [Prelude.Text])
attachInstances_instanceIds = Lens.lens (\AttachInstances' {instanceIds} -> instanceIds) (\s@AttachInstances' {} a -> s {instanceIds = a} :: AttachInstances) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
attachInstances_autoScalingGroupName :: Lens.Lens' AttachInstances Prelude.Text
attachInstances_autoScalingGroupName = Lens.lens (\AttachInstances' {autoScalingGroupName} -> autoScalingGroupName) (\s@AttachInstances' {} a -> s {autoScalingGroupName = a} :: AttachInstances)

instance Core.AWSRequest AttachInstances where
  type
    AWSResponse AttachInstances =
      AttachInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AttachInstancesResponse'

instance Prelude.Hashable AttachInstances where
  hashWithSalt _salt AttachInstances' {..} =
    _salt
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData AttachInstances where
  rnf AttachInstances' {..} =
    Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders AttachInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachInstances where
  toQuery AttachInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AttachInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "InstanceIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> instanceIds),
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newAttachInstancesResponse' smart constructor.
data AttachInstancesResponse = AttachInstancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachInstancesResponse ::
  AttachInstancesResponse
newAttachInstancesResponse = AttachInstancesResponse'

instance Prelude.NFData AttachInstancesResponse where
  rnf _ = ()
