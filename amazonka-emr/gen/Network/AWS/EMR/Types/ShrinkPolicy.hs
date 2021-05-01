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
-- Module      : Network.AWS.EMR.Types.ShrinkPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ShrinkPolicy where

import Network.AWS.EMR.Types.InstanceResizePolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Policy for customizing shrink operations. Allows configuration of
-- decommissioning timeout and targeted instance shrinking.
--
-- /See:/ 'newShrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
  { -- | Custom policy for requesting termination protection or termination of
    -- specific instances when shrinking an instance group.
    instanceResizePolicy :: Prelude.Maybe InstanceResizePolicy,
    -- | The desired timeout for decommissioning an instance. Overrides the
    -- default YARN decommissioning timeout.
    decommissionTimeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceResizePolicy =
        Prelude.Nothing,
      decommissionTimeout = Prelude.Nothing
    }

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
shrinkPolicy_instanceResizePolicy :: Lens.Lens' ShrinkPolicy (Prelude.Maybe InstanceResizePolicy)
shrinkPolicy_instanceResizePolicy = Lens.lens (\ShrinkPolicy' {instanceResizePolicy} -> instanceResizePolicy) (\s@ShrinkPolicy' {} a -> s {instanceResizePolicy = a} :: ShrinkPolicy)

-- | The desired timeout for decommissioning an instance. Overrides the
-- default YARN decommissioning timeout.
shrinkPolicy_decommissionTimeout :: Lens.Lens' ShrinkPolicy (Prelude.Maybe Prelude.Int)
shrinkPolicy_decommissionTimeout = Lens.lens (\ShrinkPolicy' {decommissionTimeout} -> decommissionTimeout) (\s@ShrinkPolicy' {} a -> s {decommissionTimeout = a} :: ShrinkPolicy)

instance Prelude.FromJSON ShrinkPolicy where
  parseJSON =
    Prelude.withObject
      "ShrinkPolicy"
      ( \x ->
          ShrinkPolicy'
            Prelude.<$> (x Prelude..:? "InstanceResizePolicy")
            Prelude.<*> (x Prelude..:? "DecommissionTimeout")
      )

instance Prelude.Hashable ShrinkPolicy

instance Prelude.NFData ShrinkPolicy

instance Prelude.ToJSON ShrinkPolicy where
  toJSON ShrinkPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceResizePolicy" Prelude..=)
              Prelude.<$> instanceResizePolicy,
            ("DecommissionTimeout" Prelude..=)
              Prelude.<$> decommissionTimeout
          ]
      )
