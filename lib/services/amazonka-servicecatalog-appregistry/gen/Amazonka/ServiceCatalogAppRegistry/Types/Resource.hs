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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The service integration information about the resource.
    integrations :: Prelude.Maybe ResourceIntegrations,
    -- | The Amazon resource name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the resource was associated with the application.
    associationTime :: Prelude.Maybe Data.POSIX
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
-- 'name', 'resource_name' - The name of the resource.
--
-- 'integrations', 'resource_integrations' - The service integration information about the resource.
--
-- 'arn', 'resource_arn' - The Amazon resource name (ARN) of the resource.
--
-- 'associationTime', 'resource_associationTime' - The time the resource was associated with the application.
newResource ::
  Resource
newResource =
  Resource'
    { name = Prelude.Nothing,
      integrations = Prelude.Nothing,
      arn = Prelude.Nothing,
      associationTime = Prelude.Nothing
    }

-- | The name of the resource.
resource_name :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | The service integration information about the resource.
resource_integrations :: Lens.Lens' Resource (Prelude.Maybe ResourceIntegrations)
resource_integrations = Lens.lens (\Resource' {integrations} -> integrations) (\s@Resource' {} a -> s {integrations = a} :: Resource)

-- | The Amazon resource name (ARN) of the resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The time the resource was associated with the application.
resource_associationTime :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_associationTime = Lens.lens (\Resource' {associationTime} -> associationTime) (\s@Resource' {} a -> s {associationTime = a} :: Resource) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "integrations")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "associationTime")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` integrations
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` associationTime

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf integrations
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associationTime
