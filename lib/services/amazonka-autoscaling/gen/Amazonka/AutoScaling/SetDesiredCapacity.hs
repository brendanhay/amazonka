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
-- Module      : Amazonka.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the size of the specified Auto Scaling group.
--
-- If a scale-in activity occurs as a result of a new @DesiredCapacity@
-- value that is lower than the current size of the group, the Auto Scaling
-- group uses its termination policy to determine which instances to
-- terminate.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-manual-scaling.html Manual scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.SetDesiredCapacity
  ( -- * Creating a Request
    SetDesiredCapacity (..),
    newSetDesiredCapacity,

    -- * Request Lenses
    setDesiredCapacity_honorCooldown,
    setDesiredCapacity_autoScalingGroupName,
    setDesiredCapacity_desiredCapacity,

    -- * Destructuring the Response
    SetDesiredCapacityResponse (..),
    newSetDesiredCapacityResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetDesiredCapacity' smart constructor.
data SetDesiredCapacity = SetDesiredCapacity'
  { -- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
    -- to complete before initiating a scaling activity to set your Auto
    -- Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling
    -- does not honor the cooldown period during manual scaling activities.
    honorCooldown :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after this operation completes and the capacity it attempts to maintain.
    desiredCapacity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDesiredCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'honorCooldown', 'setDesiredCapacity_honorCooldown' - Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
-- to complete before initiating a scaling activity to set your Auto
-- Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling
-- does not honor the cooldown period during manual scaling activities.
--
-- 'autoScalingGroupName', 'setDesiredCapacity_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'desiredCapacity', 'setDesiredCapacity_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
newSetDesiredCapacity ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'desiredCapacity'
  Prelude.Int ->
  SetDesiredCapacity
newSetDesiredCapacity
  pAutoScalingGroupName_
  pDesiredCapacity_ =
    SetDesiredCapacity'
      { honorCooldown =
          Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        desiredCapacity = pDesiredCapacity_
      }

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
-- to complete before initiating a scaling activity to set your Auto
-- Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling
-- does not honor the cooldown period during manual scaling activities.
setDesiredCapacity_honorCooldown :: Lens.Lens' SetDesiredCapacity (Prelude.Maybe Prelude.Bool)
setDesiredCapacity_honorCooldown = Lens.lens (\SetDesiredCapacity' {honorCooldown} -> honorCooldown) (\s@SetDesiredCapacity' {} a -> s {honorCooldown = a} :: SetDesiredCapacity)

-- | The name of the Auto Scaling group.
setDesiredCapacity_autoScalingGroupName :: Lens.Lens' SetDesiredCapacity Prelude.Text
setDesiredCapacity_autoScalingGroupName = Lens.lens (\SetDesiredCapacity' {autoScalingGroupName} -> autoScalingGroupName) (\s@SetDesiredCapacity' {} a -> s {autoScalingGroupName = a} :: SetDesiredCapacity)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
setDesiredCapacity_desiredCapacity :: Lens.Lens' SetDesiredCapacity Prelude.Int
setDesiredCapacity_desiredCapacity = Lens.lens (\SetDesiredCapacity' {desiredCapacity} -> desiredCapacity) (\s@SetDesiredCapacity' {} a -> s {desiredCapacity = a} :: SetDesiredCapacity)

instance Core.AWSRequest SetDesiredCapacity where
  type
    AWSResponse SetDesiredCapacity =
      SetDesiredCapacityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull SetDesiredCapacityResponse'

instance Prelude.Hashable SetDesiredCapacity where
  hashWithSalt _salt SetDesiredCapacity' {..} =
    _salt
      `Prelude.hashWithSalt` honorCooldown
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` desiredCapacity

instance Prelude.NFData SetDesiredCapacity where
  rnf SetDesiredCapacity' {..} =
    Prelude.rnf honorCooldown `Prelude.seq`
      Prelude.rnf autoScalingGroupName `Prelude.seq`
        Prelude.rnf desiredCapacity

instance Data.ToHeaders SetDesiredCapacity where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetDesiredCapacity where
  toPath = Prelude.const "/"

instance Data.ToQuery SetDesiredCapacity where
  toQuery SetDesiredCapacity' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetDesiredCapacity" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "HonorCooldown" Data.=: honorCooldown,
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "DesiredCapacity" Data.=: desiredCapacity
      ]

-- | /See:/ 'newSetDesiredCapacityResponse' smart constructor.
data SetDesiredCapacityResponse = SetDesiredCapacityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDesiredCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetDesiredCapacityResponse ::
  SetDesiredCapacityResponse
newSetDesiredCapacityResponse =
  SetDesiredCapacityResponse'

instance Prelude.NFData SetDesiredCapacityResponse where
  rnf _ = ()
