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
-- Module      : Amazonka.EC2.Types.ResourceStatementRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ResourceStatementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource statement.
--
-- /See:/ 'newResourceStatementRequest' smart constructor.
data ResourceStatementRequest = ResourceStatementRequest'
  { -- | The resource types.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The resources.
    resources :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceStatementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypes', 'resourceStatementRequest_resourceTypes' - The resource types.
--
-- 'resources', 'resourceStatementRequest_resources' - The resources.
newResourceStatementRequest ::
  ResourceStatementRequest
newResourceStatementRequest =
  ResourceStatementRequest'
    { resourceTypes =
        Prelude.Nothing,
      resources = Prelude.Nothing
    }

-- | The resource types.
resourceStatementRequest_resourceTypes :: Lens.Lens' ResourceStatementRequest (Prelude.Maybe [Prelude.Text])
resourceStatementRequest_resourceTypes = Lens.lens (\ResourceStatementRequest' {resourceTypes} -> resourceTypes) (\s@ResourceStatementRequest' {} a -> s {resourceTypes = a} :: ResourceStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The resources.
resourceStatementRequest_resources :: Lens.Lens' ResourceStatementRequest (Prelude.Maybe [Prelude.Text])
resourceStatementRequest_resources = Lens.lens (\ResourceStatementRequest' {resources} -> resources) (\s@ResourceStatementRequest' {} a -> s {resources = a} :: ResourceStatementRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ResourceStatementRequest where
  hashWithSalt _salt ResourceStatementRequest' {..} =
    _salt
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` resources

instance Prelude.NFData ResourceStatementRequest where
  rnf ResourceStatementRequest' {..} =
    Prelude.rnf resourceTypes `Prelude.seq`
      Prelude.rnf resources

instance Data.ToQuery ResourceStatementRequest where
  toQuery ResourceStatementRequest' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "ResourceType"
              Prelude.<$> resourceTypes
          ),
        Data.toQuery
          (Data.toQueryList "Resource" Prelude.<$> resources)
      ]
