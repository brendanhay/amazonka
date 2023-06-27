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
-- Module      : Amazonka.Config.Types.ExclusionByResourceTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ExclusionByResourceTypes where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether the configuration recorder excludes resource types
-- from being recorded. Use the @resourceTypes@ field to enter a
-- comma-separated list of resource types to exclude as exemptions.
--
-- /See:/ 'newExclusionByResourceTypes' smart constructor.
data ExclusionByResourceTypes = ExclusionByResourceTypes'
  { -- | A comma-separated list of resource types to exclude from recording by
    -- the configuration recorder.
    resourceTypes :: Prelude.Maybe [ResourceType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExclusionByResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypes', 'exclusionByResourceTypes_resourceTypes' - A comma-separated list of resource types to exclude from recording by
-- the configuration recorder.
newExclusionByResourceTypes ::
  ExclusionByResourceTypes
newExclusionByResourceTypes =
  ExclusionByResourceTypes'
    { resourceTypes =
        Prelude.Nothing
    }

-- | A comma-separated list of resource types to exclude from recording by
-- the configuration recorder.
exclusionByResourceTypes_resourceTypes :: Lens.Lens' ExclusionByResourceTypes (Prelude.Maybe [ResourceType])
exclusionByResourceTypes_resourceTypes = Lens.lens (\ExclusionByResourceTypes' {resourceTypes} -> resourceTypes) (\s@ExclusionByResourceTypes' {} a -> s {resourceTypes = a} :: ExclusionByResourceTypes) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExclusionByResourceTypes where
  parseJSON =
    Data.withObject
      "ExclusionByResourceTypes"
      ( \x ->
          ExclusionByResourceTypes'
            Prelude.<$> (x Data..:? "resourceTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExclusionByResourceTypes where
  hashWithSalt _salt ExclusionByResourceTypes' {..} =
    _salt `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData ExclusionByResourceTypes where
  rnf ExclusionByResourceTypes' {..} =
    Prelude.rnf resourceTypes

instance Data.ToJSON ExclusionByResourceTypes where
  toJSON ExclusionByResourceTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceTypes" Data..=)
              Prelude.<$> resourceTypes
          ]
      )
