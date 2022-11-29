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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingListener where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | A complex type for a listener for a custom routing accelerator.
--
-- /See:/ 'newCustomRoutingListener' smart constructor.
data CustomRoutingListener = CustomRoutingListener'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Maybe Prelude.Text,
    -- | The port range to support for connections from clients to your
    -- accelerator.
    --
    -- Separately, you set port ranges for endpoints. For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
    portRanges :: Prelude.Maybe (Prelude.NonEmpty PortRange)
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
-- 'listenerArn', 'customRoutingListener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'portRanges', 'customRoutingListener_portRanges' - The port range to support for connections from clients to your
-- accelerator.
--
-- Separately, you set port ranges for endpoints. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
newCustomRoutingListener ::
  CustomRoutingListener
newCustomRoutingListener =
  CustomRoutingListener'
    { listenerArn =
        Prelude.Nothing,
      portRanges = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
customRoutingListener_listenerArn :: Lens.Lens' CustomRoutingListener (Prelude.Maybe Prelude.Text)
customRoutingListener_listenerArn = Lens.lens (\CustomRoutingListener' {listenerArn} -> listenerArn) (\s@CustomRoutingListener' {} a -> s {listenerArn = a} :: CustomRoutingListener)

-- | The port range to support for connections from clients to your
-- accelerator.
--
-- Separately, you set port ranges for endpoints. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-custom-routing-endpoints.html About endpoints for custom routing accelerators>.
customRoutingListener_portRanges :: Lens.Lens' CustomRoutingListener (Prelude.Maybe (Prelude.NonEmpty PortRange))
customRoutingListener_portRanges = Lens.lens (\CustomRoutingListener' {portRanges} -> portRanges) (\s@CustomRoutingListener' {} a -> s {portRanges = a} :: CustomRoutingListener) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CustomRoutingListener where
  parseJSON =
    Core.withObject
      "CustomRoutingListener"
      ( \x ->
          CustomRoutingListener'
            Prelude.<$> (x Core..:? "ListenerArn")
            Prelude.<*> (x Core..:? "PortRanges")
      )

instance Prelude.Hashable CustomRoutingListener where
  hashWithSalt _salt CustomRoutingListener' {..} =
    _salt `Prelude.hashWithSalt` listenerArn
      `Prelude.hashWithSalt` portRanges

instance Prelude.NFData CustomRoutingListener where
  rnf CustomRoutingListener' {..} =
    Prelude.rnf listenerArn
      `Prelude.seq` Prelude.rnf portRanges
