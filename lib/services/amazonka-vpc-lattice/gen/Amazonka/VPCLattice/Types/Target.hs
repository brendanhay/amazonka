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
-- Module      : Amazonka.VPCLattice.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a target.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | The port on which the target is listening. For HTTP, the default is
    -- @80@. For HTTPS, the default is @443@.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the target. If the target type of the target group is
    -- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
    -- an IP address. If the target type is @LAMBDA@, this is the ARN of the
    -- Lambda function. If the target type is @ALB@, this is the ARN of the
    -- Application Load Balancer.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'target_port' - The port on which the target is listening. For HTTP, the default is
-- @80@. For HTTPS, the default is @443@.
--
-- 'id', 'target_id' - The ID of the target. If the target type of the target group is
-- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
-- an IP address. If the target type is @LAMBDA@, this is the ARN of the
-- Lambda function. If the target type is @ALB@, this is the ARN of the
-- Application Load Balancer.
newTarget ::
  -- | 'id'
  Prelude.Text ->
  Target
newTarget pId_ =
  Target' {port = Prelude.Nothing, id = pId_}

-- | The port on which the target is listening. For HTTP, the default is
-- @80@. For HTTPS, the default is @443@.
target_port :: Lens.Lens' Target (Prelude.Maybe Prelude.Natural)
target_port = Lens.lens (\Target' {port} -> port) (\s@Target' {} a -> s {port = a} :: Target)

-- | The ID of the target. If the target type of the target group is
-- @INSTANCE@, this is an instance ID. If the target type is @IP@ , this is
-- an IP address. If the target type is @LAMBDA@, this is the ARN of the
-- Lambda function. If the target type is @ALB@, this is the ARN of the
-- Application Load Balancer.
target_id :: Lens.Lens' Target Prelude.Text
target_id = Lens.lens (\Target' {id} -> id) (\s@Target' {} a -> s {id = a} :: Target)

instance Data.FromJSON Target where
  parseJSON =
    Data.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Data..:? "port")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` id

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf id

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("port" Data..=) Prelude.<$> port,
            Prelude.Just ("id" Data..= id)
          ]
      )
