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
-- Module      : Amazonka.VPCLattice.Types.TargetFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a target failure.
--
-- /See:/ 'newTargetFailure' smart constructor.
data TargetFailure = TargetFailure'
  { -- | The failure code.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The failure message.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target. If the target type of the target group is
    -- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
    -- an IP address. If the target type is @LAMBDA@, this is the ARN of the
    -- Lambda function. If the target type is @ALB@, this is the ARN of the
    -- Application Load Balancer.
    id :: Prelude.Maybe Prelude.Text,
    -- | The port on which the target is listening. This parameter doesn\'t apply
    -- if the target is a Lambda function.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'targetFailure_failureCode' - The failure code.
--
-- 'failureMessage', 'targetFailure_failureMessage' - The failure message.
--
-- 'id', 'targetFailure_id' - The ID of the target. If the target type of the target group is
-- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
-- an IP address. If the target type is @LAMBDA@, this is the ARN of the
-- Lambda function. If the target type is @ALB@, this is the ARN of the
-- Application Load Balancer.
--
-- 'port', 'targetFailure_port' - The port on which the target is listening. This parameter doesn\'t apply
-- if the target is a Lambda function.
newTargetFailure ::
  TargetFailure
newTargetFailure =
  TargetFailure'
    { failureCode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The failure code.
targetFailure_failureCode :: Lens.Lens' TargetFailure (Prelude.Maybe Prelude.Text)
targetFailure_failureCode = Lens.lens (\TargetFailure' {failureCode} -> failureCode) (\s@TargetFailure' {} a -> s {failureCode = a} :: TargetFailure)

-- | The failure message.
targetFailure_failureMessage :: Lens.Lens' TargetFailure (Prelude.Maybe Prelude.Text)
targetFailure_failureMessage = Lens.lens (\TargetFailure' {failureMessage} -> failureMessage) (\s@TargetFailure' {} a -> s {failureMessage = a} :: TargetFailure)

-- | The ID of the target. If the target type of the target group is
-- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
-- an IP address. If the target type is @LAMBDA@, this is the ARN of the
-- Lambda function. If the target type is @ALB@, this is the ARN of the
-- Application Load Balancer.
targetFailure_id :: Lens.Lens' TargetFailure (Prelude.Maybe Prelude.Text)
targetFailure_id = Lens.lens (\TargetFailure' {id} -> id) (\s@TargetFailure' {} a -> s {id = a} :: TargetFailure)

-- | The port on which the target is listening. This parameter doesn\'t apply
-- if the target is a Lambda function.
targetFailure_port :: Lens.Lens' TargetFailure (Prelude.Maybe Prelude.Natural)
targetFailure_port = Lens.lens (\TargetFailure' {port} -> port) (\s@TargetFailure' {} a -> s {port = a} :: TargetFailure)

instance Data.FromJSON TargetFailure where
  parseJSON =
    Data.withObject
      "TargetFailure"
      ( \x ->
          TargetFailure'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..:? "failureMessage")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "port")
      )

instance Prelude.Hashable TargetFailure where
  hashWithSalt _salt TargetFailure' {..} =
    _salt
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` port

instance Prelude.NFData TargetFailure where
  rnf TargetFailure' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf port
