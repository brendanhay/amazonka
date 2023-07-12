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
-- Module      : Amazonka.AutoScaling.Types.InstanceReusePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceReusePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance reuse policy for a warm pool.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-warm-pools.html Warm pools for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newInstanceReusePolicy' smart constructor.
data InstanceReusePolicy = InstanceReusePolicy'
  { -- | Specifies whether instances in the Auto Scaling group can be returned to
    -- the warm pool on scale in.
    reuseOnScaleIn :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceReusePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reuseOnScaleIn', 'instanceReusePolicy_reuseOnScaleIn' - Specifies whether instances in the Auto Scaling group can be returned to
-- the warm pool on scale in.
newInstanceReusePolicy ::
  InstanceReusePolicy
newInstanceReusePolicy =
  InstanceReusePolicy'
    { reuseOnScaleIn =
        Prelude.Nothing
    }

-- | Specifies whether instances in the Auto Scaling group can be returned to
-- the warm pool on scale in.
instanceReusePolicy_reuseOnScaleIn :: Lens.Lens' InstanceReusePolicy (Prelude.Maybe Prelude.Bool)
instanceReusePolicy_reuseOnScaleIn = Lens.lens (\InstanceReusePolicy' {reuseOnScaleIn} -> reuseOnScaleIn) (\s@InstanceReusePolicy' {} a -> s {reuseOnScaleIn = a} :: InstanceReusePolicy)

instance Data.FromXML InstanceReusePolicy where
  parseXML x =
    InstanceReusePolicy'
      Prelude.<$> (x Data..@? "ReuseOnScaleIn")

instance Prelude.Hashable InstanceReusePolicy where
  hashWithSalt _salt InstanceReusePolicy' {..} =
    _salt `Prelude.hashWithSalt` reuseOnScaleIn

instance Prelude.NFData InstanceReusePolicy where
  rnf InstanceReusePolicy' {..} =
    Prelude.rnf reuseOnScaleIn

instance Data.ToQuery InstanceReusePolicy where
  toQuery InstanceReusePolicy' {..} =
    Prelude.mconcat
      ["ReuseOnScaleIn" Data.=: reuseOnScaleIn]
