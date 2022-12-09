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
-- Module      : Amazonka.EMR.Types.ShrinkPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ShrinkPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceResizePolicy
import qualified Amazonka.Prelude as Prelude

-- | Policy for customizing shrink operations. Allows configuration of
-- decommissioning timeout and targeted instance shrinking.
--
-- /See:/ 'newShrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
  { -- | The desired timeout for decommissioning an instance. Overrides the
    -- default YARN decommissioning timeout.
    decommissionTimeout :: Prelude.Maybe Prelude.Int,
    -- | Custom policy for requesting termination protection or termination of
    -- specific instances when shrinking an instance group.
    instanceResizePolicy :: Prelude.Maybe InstanceResizePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShrinkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decommissionTimeout', 'shrinkPolicy_decommissionTimeout' - The desired timeout for decommissioning an instance. Overrides the
-- default YARN decommissioning timeout.
--
-- 'instanceResizePolicy', 'shrinkPolicy_instanceResizePolicy' - Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
newShrinkPolicy ::
  ShrinkPolicy
newShrinkPolicy =
  ShrinkPolicy'
    { decommissionTimeout =
        Prelude.Nothing,
      instanceResizePolicy = Prelude.Nothing
    }

-- | The desired timeout for decommissioning an instance. Overrides the
-- default YARN decommissioning timeout.
shrinkPolicy_decommissionTimeout :: Lens.Lens' ShrinkPolicy (Prelude.Maybe Prelude.Int)
shrinkPolicy_decommissionTimeout = Lens.lens (\ShrinkPolicy' {decommissionTimeout} -> decommissionTimeout) (\s@ShrinkPolicy' {} a -> s {decommissionTimeout = a} :: ShrinkPolicy)

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
shrinkPolicy_instanceResizePolicy :: Lens.Lens' ShrinkPolicy (Prelude.Maybe InstanceResizePolicy)
shrinkPolicy_instanceResizePolicy = Lens.lens (\ShrinkPolicy' {instanceResizePolicy} -> instanceResizePolicy) (\s@ShrinkPolicy' {} a -> s {instanceResizePolicy = a} :: ShrinkPolicy)

instance Data.FromJSON ShrinkPolicy where
  parseJSON =
    Data.withObject
      "ShrinkPolicy"
      ( \x ->
          ShrinkPolicy'
            Prelude.<$> (x Data..:? "DecommissionTimeout")
            Prelude.<*> (x Data..:? "InstanceResizePolicy")
      )

instance Prelude.Hashable ShrinkPolicy where
  hashWithSalt _salt ShrinkPolicy' {..} =
    _salt `Prelude.hashWithSalt` decommissionTimeout
      `Prelude.hashWithSalt` instanceResizePolicy

instance Prelude.NFData ShrinkPolicy where
  rnf ShrinkPolicy' {..} =
    Prelude.rnf decommissionTimeout
      `Prelude.seq` Prelude.rnf instanceResizePolicy

instance Data.ToJSON ShrinkPolicy where
  toJSON ShrinkPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DecommissionTimeout" Data..=)
              Prelude.<$> decommissionTimeout,
            ("InstanceResizePolicy" Data..=)
              Prelude.<$> instanceResizePolicy
          ]
      )
