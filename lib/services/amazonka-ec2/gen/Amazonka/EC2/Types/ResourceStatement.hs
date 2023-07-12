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
-- Module      : Amazonka.EC2.Types.ResourceStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ResourceStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource statement.
--
-- /See:/ 'newResourceStatement' smart constructor.
data ResourceStatement = ResourceStatement'
  { -- | The resource types.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The resources.
    resources :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypes', 'resourceStatement_resourceTypes' - The resource types.
--
-- 'resources', 'resourceStatement_resources' - The resources.
newResourceStatement ::
  ResourceStatement
newResourceStatement =
  ResourceStatement'
    { resourceTypes = Prelude.Nothing,
      resources = Prelude.Nothing
    }

-- | The resource types.
resourceStatement_resourceTypes :: Lens.Lens' ResourceStatement (Prelude.Maybe [Prelude.Text])
resourceStatement_resourceTypes = Lens.lens (\ResourceStatement' {resourceTypes} -> resourceTypes) (\s@ResourceStatement' {} a -> s {resourceTypes = a} :: ResourceStatement) Prelude.. Lens.mapping Lens.coerced

-- | The resources.
resourceStatement_resources :: Lens.Lens' ResourceStatement (Prelude.Maybe [Prelude.Text])
resourceStatement_resources = Lens.lens (\ResourceStatement' {resources} -> resources) (\s@ResourceStatement' {} a -> s {resources = a} :: ResourceStatement) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ResourceStatement where
  parseXML x =
    ResourceStatement'
      Prelude.<$> ( x
                      Data..@? "resourceTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "resourceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable ResourceStatement where
  hashWithSalt _salt ResourceStatement' {..} =
    _salt
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` resources

instance Prelude.NFData ResourceStatement where
  rnf ResourceStatement' {..} =
    Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf resources
