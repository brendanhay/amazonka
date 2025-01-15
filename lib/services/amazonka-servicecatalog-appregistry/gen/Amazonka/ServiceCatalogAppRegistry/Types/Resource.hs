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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceIntegrations

-- | The information about the resource.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The Amazon resource name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the resource was associated with the application.
    associationTime :: Prelude.Maybe Data.ISO8601,
    -- | The service integration information about the resource.
    integrations :: Prelude.Maybe ResourceIntegrations,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resource_arn' - The Amazon resource name (ARN) of the resource.
--
-- 'associationTime', 'resource_associationTime' - The time the resource was associated with the application.
--
-- 'integrations', 'resource_integrations' - The service integration information about the resource.
--
-- 'name', 'resource_name' - The name of the resource.
newResource ::
  Resource
newResource =
  Resource'
    { arn = Prelude.Nothing,
      associationTime = Prelude.Nothing,
      integrations = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) of the resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The time the resource was associated with the application.
resource_associationTime :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_associationTime = Lens.lens (\Resource' {associationTime} -> associationTime) (\s@Resource' {} a -> s {associationTime = a} :: Resource) Prelude.. Lens.mapping Data._Time

-- | The service integration information about the resource.
resource_integrations :: Lens.Lens' Resource (Prelude.Maybe ResourceIntegrations)
resource_integrations = Lens.lens (\Resource' {integrations} -> integrations) (\s@Resource' {} a -> s {integrations = a} :: Resource)

-- | The name of the resource.
resource_name :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "associationTime")
            Prelude.<*> (x Data..:? "integrations")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` associationTime
      `Prelude.hashWithSalt` integrations
      `Prelude.hashWithSalt` name

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf associationTime `Prelude.seq`
        Prelude.rnf integrations `Prelude.seq`
          Prelude.rnf name
