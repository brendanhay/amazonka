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
-- Module      : Amazonka.AlexaBusiness.Types.GatewayGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.GatewayGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of a gateway group.
--
-- /See:/ 'newGatewayGroupSummary' smart constructor.
data GatewayGroupSummary = GatewayGroupSummary'
  { -- | The ARN of the gateway group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the gateway group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway group.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'gatewayGroupSummary_description' - The description of the gateway group.
--
-- 'name', 'gatewayGroupSummary_name' - The name of the gateway group.
newGatewayGroupSummary ::
  GatewayGroupSummary
newGatewayGroupSummary =
  GatewayGroupSummary'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the gateway group.
gatewayGroupSummary_arn :: Lens.Lens' GatewayGroupSummary (Prelude.Maybe Prelude.Text)
gatewayGroupSummary_arn = Lens.lens (\GatewayGroupSummary' {arn} -> arn) (\s@GatewayGroupSummary' {} a -> s {arn = a} :: GatewayGroupSummary)

-- | The description of the gateway group.
gatewayGroupSummary_description :: Lens.Lens' GatewayGroupSummary (Prelude.Maybe Prelude.Text)
gatewayGroupSummary_description = Lens.lens (\GatewayGroupSummary' {description} -> description) (\s@GatewayGroupSummary' {} a -> s {description = a} :: GatewayGroupSummary)

-- | The name of the gateway group.
gatewayGroupSummary_name :: Lens.Lens' GatewayGroupSummary (Prelude.Maybe Prelude.Text)
gatewayGroupSummary_name = Lens.lens (\GatewayGroupSummary' {name} -> name) (\s@GatewayGroupSummary' {} a -> s {name = a} :: GatewayGroupSummary)

instance Data.FromJSON GatewayGroupSummary where
  parseJSON =
    Data.withObject
      "GatewayGroupSummary"
      ( \x ->
          GatewayGroupSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable GatewayGroupSummary where
  hashWithSalt _salt GatewayGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData GatewayGroupSummary where
  rnf GatewayGroupSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
