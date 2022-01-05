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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingListener where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types.PortRange
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type for a listener for a custom routing accelerator.
--
-- /See:/ 'newCustomRoutingListener' smart constructor.
data CustomRoutingListener = CustomRoutingListener'
  { -- | The port range to support for connections from clients to your
    -- accelerator.
    --
    -- Separately, you set port ranges for endpoints. For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
    portRanges :: Prelude.Maybe (Prelude.NonEmpty PortRange),
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portRanges', 'customRoutingListener_portRanges' - The port range to support for connections from clients to your
-- accelerator.
--
-- Separately, you set port ranges for endpoints. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
--
-- 'listenerArn', 'customRoutingListener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
newCustomRoutingListener ::
  CustomRoutingListener
newCustomRoutingListener =
  CustomRoutingListener'
    { portRanges =
        Prelude.Nothing,
      listenerArn = Prelude.Nothing
    }

-- | The port range to support for connections from clients to your
-- accelerator.
--
-- Separately, you set port ranges for endpoints. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
customRoutingListener_portRanges :: Lens.Lens' CustomRoutingListener (Prelude.Maybe (Prelude.NonEmpty PortRange))
customRoutingListener_portRanges = Lens.lens (\CustomRoutingListener' {portRanges} -> portRanges) (\s@CustomRoutingListener' {} a -> s {portRanges = a} :: CustomRoutingListener) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the listener.
customRoutingListener_listenerArn :: Lens.Lens' CustomRoutingListener (Prelude.Maybe Prelude.Text)
customRoutingListener_listenerArn = Lens.lens (\CustomRoutingListener' {listenerArn} -> listenerArn) (\s@CustomRoutingListener' {} a -> s {listenerArn = a} :: CustomRoutingListener)

instance Core.FromJSON CustomRoutingListener where
  parseJSON =
    Core.withObject
      "CustomRoutingListener"
      ( \x ->
          CustomRoutingListener'
            Prelude.<$> (x Core..:? "PortRanges")
            Prelude.<*> (x Core..:? "ListenerArn")
      )

instance Prelude.Hashable CustomRoutingListener where
  hashWithSalt _salt CustomRoutingListener' {..} =
    _salt `Prelude.hashWithSalt` portRanges
      `Prelude.hashWithSalt` listenerArn

instance Prelude.NFData CustomRoutingListener where
  rnf CustomRoutingListener' {..} =
    Prelude.rnf portRanges
      `Prelude.seq` Prelude.rnf listenerArn
