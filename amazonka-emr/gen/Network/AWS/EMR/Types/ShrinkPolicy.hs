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
-- Module      : Network.AWS.EMR.Types.ShrinkPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ShrinkPolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.InstanceResizePolicy
import qualified Network.AWS.Lens as Lens

-- | Policy for customizing shrink operations. Allows configuration of
-- decommissioning timeout and targeted instance shrinking.
--
-- /See:/ 'newShrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
  { -- | Custom policy for requesting termination protection or termination of
    -- specific instances when shrinking an instance group.
    instanceResizePolicy :: Core.Maybe InstanceResizePolicy,
    -- | The desired timeout for decommissioning an instance. Overrides the
    -- default YARN decommissioning timeout.
    decommissionTimeout :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ShrinkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceResizePolicy', 'shrinkPolicy_instanceResizePolicy' - Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
--
-- 'decommissionTimeout', 'shrinkPolicy_decommissionTimeout' - The desired timeout for decommissioning an instance. Overrides the
-- default YARN decommissioning timeout.
newShrinkPolicy ::
  ShrinkPolicy
newShrinkPolicy =
  ShrinkPolicy'
    { instanceResizePolicy = Core.Nothing,
      decommissionTimeout = Core.Nothing
    }

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
shrinkPolicy_instanceResizePolicy :: Lens.Lens' ShrinkPolicy (Core.Maybe InstanceResizePolicy)
shrinkPolicy_instanceResizePolicy = Lens.lens (\ShrinkPolicy' {instanceResizePolicy} -> instanceResizePolicy) (\s@ShrinkPolicy' {} a -> s {instanceResizePolicy = a} :: ShrinkPolicy)

-- | The desired timeout for decommissioning an instance. Overrides the
-- default YARN decommissioning timeout.
shrinkPolicy_decommissionTimeout :: Lens.Lens' ShrinkPolicy (Core.Maybe Core.Int)
shrinkPolicy_decommissionTimeout = Lens.lens (\ShrinkPolicy' {decommissionTimeout} -> decommissionTimeout) (\s@ShrinkPolicy' {} a -> s {decommissionTimeout = a} :: ShrinkPolicy)

instance Core.FromJSON ShrinkPolicy where
  parseJSON =
    Core.withObject
      "ShrinkPolicy"
      ( \x ->
          ShrinkPolicy'
            Core.<$> (x Core..:? "InstanceResizePolicy")
            Core.<*> (x Core..:? "DecommissionTimeout")
      )

instance Core.Hashable ShrinkPolicy

instance Core.NFData ShrinkPolicy

instance Core.ToJSON ShrinkPolicy where
  toJSON ShrinkPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceResizePolicy" Core..=)
              Core.<$> instanceResizePolicy,
            ("DecommissionTimeout" Core..=)
              Core.<$> decommissionTimeout
          ]
      )
