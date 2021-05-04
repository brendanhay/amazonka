{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the load-based auto scaling configuration for a specified layer.
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances>.
--
-- To use load-based auto scaling, you must create a set of load-based auto
-- scaling instances. Load-based auto scaling operates only on the
-- instances from that set, so you must ensure that you have created enough
-- instances to handle the maximum anticipated load.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
  ( -- * Creating a Request
    SetLoadBasedAutoScaling (..),
    newSetLoadBasedAutoScaling,

    -- * Request Lenses
    setLoadBasedAutoScaling_downScaling,
    setLoadBasedAutoScaling_enable,
    setLoadBasedAutoScaling_upScaling,
    setLoadBasedAutoScaling_layerId,

    -- * Destructuring the Response
    SetLoadBasedAutoScalingResponse (..),
    newSetLoadBasedAutoScalingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetLoadBasedAutoScaling' smart constructor.
data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling'
  { -- | An @AutoScalingThresholds@ object with the downscaling threshold
    -- configuration. If the load falls below these thresholds for a specified
    -- amount of time, AWS OpsWorks Stacks stops a specified number of
    -- instances.
    downScaling :: Prelude.Maybe AutoScalingThresholds,
    -- | Enables load-based auto scaling for the layer.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | An @AutoScalingThresholds@ object with the upscaling threshold
    -- configuration. If the load exceeds these thresholds for a specified
    -- amount of time, AWS OpsWorks Stacks starts a specified number of
    -- instances.
    upScaling :: Prelude.Maybe AutoScalingThresholds,
    -- | The layer ID.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoadBasedAutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downScaling', 'setLoadBasedAutoScaling_downScaling' - An @AutoScalingThresholds@ object with the downscaling threshold
-- configuration. If the load falls below these thresholds for a specified
-- amount of time, AWS OpsWorks Stacks stops a specified number of
-- instances.
--
-- 'enable', 'setLoadBasedAutoScaling_enable' - Enables load-based auto scaling for the layer.
--
-- 'upScaling', 'setLoadBasedAutoScaling_upScaling' - An @AutoScalingThresholds@ object with the upscaling threshold
-- configuration. If the load exceeds these thresholds for a specified
-- amount of time, AWS OpsWorks Stacks starts a specified number of
-- instances.
--
-- 'layerId', 'setLoadBasedAutoScaling_layerId' - The layer ID.
newSetLoadBasedAutoScaling ::
  -- | 'layerId'
  Prelude.Text ->
  SetLoadBasedAutoScaling
newSetLoadBasedAutoScaling pLayerId_ =
  SetLoadBasedAutoScaling'
    { downScaling =
        Prelude.Nothing,
      enable = Prelude.Nothing,
      upScaling = Prelude.Nothing,
      layerId = pLayerId_
    }

-- | An @AutoScalingThresholds@ object with the downscaling threshold
-- configuration. If the load falls below these thresholds for a specified
-- amount of time, AWS OpsWorks Stacks stops a specified number of
-- instances.
setLoadBasedAutoScaling_downScaling :: Lens.Lens' SetLoadBasedAutoScaling (Prelude.Maybe AutoScalingThresholds)
setLoadBasedAutoScaling_downScaling = Lens.lens (\SetLoadBasedAutoScaling' {downScaling} -> downScaling) (\s@SetLoadBasedAutoScaling' {} a -> s {downScaling = a} :: SetLoadBasedAutoScaling)

-- | Enables load-based auto scaling for the layer.
setLoadBasedAutoScaling_enable :: Lens.Lens' SetLoadBasedAutoScaling (Prelude.Maybe Prelude.Bool)
setLoadBasedAutoScaling_enable = Lens.lens (\SetLoadBasedAutoScaling' {enable} -> enable) (\s@SetLoadBasedAutoScaling' {} a -> s {enable = a} :: SetLoadBasedAutoScaling)

-- | An @AutoScalingThresholds@ object with the upscaling threshold
-- configuration. If the load exceeds these thresholds for a specified
-- amount of time, AWS OpsWorks Stacks starts a specified number of
-- instances.
setLoadBasedAutoScaling_upScaling :: Lens.Lens' SetLoadBasedAutoScaling (Prelude.Maybe AutoScalingThresholds)
setLoadBasedAutoScaling_upScaling = Lens.lens (\SetLoadBasedAutoScaling' {upScaling} -> upScaling) (\s@SetLoadBasedAutoScaling' {} a -> s {upScaling = a} :: SetLoadBasedAutoScaling)

-- | The layer ID.
setLoadBasedAutoScaling_layerId :: Lens.Lens' SetLoadBasedAutoScaling Prelude.Text
setLoadBasedAutoScaling_layerId = Lens.lens (\SetLoadBasedAutoScaling' {layerId} -> layerId) (\s@SetLoadBasedAutoScaling' {} a -> s {layerId = a} :: SetLoadBasedAutoScaling)

instance Prelude.AWSRequest SetLoadBasedAutoScaling where
  type
    Rs SetLoadBasedAutoScaling =
      SetLoadBasedAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      SetLoadBasedAutoScalingResponse'

instance Prelude.Hashable SetLoadBasedAutoScaling

instance Prelude.NFData SetLoadBasedAutoScaling

instance Prelude.ToHeaders SetLoadBasedAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.SetLoadBasedAutoScaling" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetLoadBasedAutoScaling where
  toJSON SetLoadBasedAutoScaling' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DownScaling" Prelude..=) Prelude.<$> downScaling,
            ("Enable" Prelude..=) Prelude.<$> enable,
            ("UpScaling" Prelude..=) Prelude.<$> upScaling,
            Prelude.Just ("LayerId" Prelude..= layerId)
          ]
      )

instance Prelude.ToPath SetLoadBasedAutoScaling where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetLoadBasedAutoScaling where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetLoadBasedAutoScalingResponse' smart constructor.
data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoadBasedAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetLoadBasedAutoScalingResponse ::
  SetLoadBasedAutoScalingResponse
newSetLoadBasedAutoScalingResponse =
  SetLoadBasedAutoScalingResponse'

instance
  Prelude.NFData
    SetLoadBasedAutoScalingResponse
