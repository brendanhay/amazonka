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
-- Module      : Amazonka.FIS.Types.TargetResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.TargetResourceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FIS.Types.TargetResourceTypeParameter
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource type.
--
-- /See:/ 'newTargetResourceType' smart constructor.
data TargetResourceType = TargetResourceType'
  { -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | A description of the resource type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the resource type.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TargetResourceTypeParameter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'targetResourceType_resourceType' - The resource type.
--
-- 'description', 'targetResourceType_description' - A description of the resource type.
--
-- 'parameters', 'targetResourceType_parameters' - The parameters for the resource type.
newTargetResourceType ::
  TargetResourceType
newTargetResourceType =
  TargetResourceType'
    { resourceType = Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The resource type.
targetResourceType_resourceType :: Lens.Lens' TargetResourceType (Prelude.Maybe Prelude.Text)
targetResourceType_resourceType = Lens.lens (\TargetResourceType' {resourceType} -> resourceType) (\s@TargetResourceType' {} a -> s {resourceType = a} :: TargetResourceType)

-- | A description of the resource type.
targetResourceType_description :: Lens.Lens' TargetResourceType (Prelude.Maybe Prelude.Text)
targetResourceType_description = Lens.lens (\TargetResourceType' {description} -> description) (\s@TargetResourceType' {} a -> s {description = a} :: TargetResourceType)

-- | The parameters for the resource type.
targetResourceType_parameters :: Lens.Lens' TargetResourceType (Prelude.Maybe (Prelude.HashMap Prelude.Text TargetResourceTypeParameter))
targetResourceType_parameters = Lens.lens (\TargetResourceType' {parameters} -> parameters) (\s@TargetResourceType' {} a -> s {parameters = a} :: TargetResourceType) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TargetResourceType where
  parseJSON =
    Core.withObject
      "TargetResourceType"
      ( \x ->
          TargetResourceType'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TargetResourceType where
  hashWithSalt _salt TargetResourceType' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData TargetResourceType where
  rnf TargetResourceType' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
