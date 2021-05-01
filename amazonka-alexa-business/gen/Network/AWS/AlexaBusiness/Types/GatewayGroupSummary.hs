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
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroupSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of a gateway group.
--
-- /See:/ 'newGatewayGroupSummary' smart constructor.
data GatewayGroupSummary = GatewayGroupSummary'
  { -- | The ARN of the gateway group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the gateway group.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GatewayGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'gatewayGroupSummary_arn' - The ARN of the gateway group.
--
-- 'name', 'gatewayGroupSummary_name' - The name of the gateway group.
--
-- 'description', 'gatewayGroupSummary_description' - The description of the gateway group.
newGatewayGroupSummary ::
  GatewayGroupSummary
newGatewayGroupSummary =
  GatewayGroupSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ARN of the gateway group.
gatewayGroupSummary_arn :: Lens.Lens' GatewayGroupSummary (Prelude.Maybe Prelude.Text)
gatewayGroupSummary_arn = Lens.lens (\GatewayGroupSummary' {arn} -> arn) (\s@GatewayGroupSummary' {} a -> s {arn = a} :: GatewayGroupSummary)

-- | The name of the gateway group.
gatewayGroupSummary_name :: Lens.Lens' GatewayGroupSummary (Prelude.Maybe Prelude.Text)
gatewayGroupSummary_name = Lens.lens (\GatewayGroupSummary' {name} -> name) (\s@GatewayGroupSummary' {} a -> s {name = a} :: GatewayGroupSummary)

-- | The description of the gateway group.
gatewayGroupSummary_description :: Lens.Lens' GatewayGroupSummary (Prelude.Maybe Prelude.Text)
gatewayGroupSummary_description = Lens.lens (\GatewayGroupSummary' {description} -> description) (\s@GatewayGroupSummary' {} a -> s {description = a} :: GatewayGroupSummary)

instance Prelude.FromJSON GatewayGroupSummary where
  parseJSON =
    Prelude.withObject
      "GatewayGroupSummary"
      ( \x ->
          GatewayGroupSummary'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable GatewayGroupSummary

instance Prelude.NFData GatewayGroupSummary
