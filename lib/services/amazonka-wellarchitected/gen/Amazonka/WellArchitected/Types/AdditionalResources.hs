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
-- Module      : Amazonka.WellArchitected.Types.AdditionalResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.AdditionalResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.AdditionalResourceType
import Amazonka.WellArchitected.Types.ChoiceContent

-- | The choice level additional resources.
--
-- /See:/ 'newAdditionalResources' smart constructor.
data AdditionalResources = AdditionalResources'
  { -- | The URLs for additional resources, either helpful resources or
    -- improvement plans. Up to five additional URLs can be specified.
    content :: Prelude.Maybe [ChoiceContent],
    -- | Type of additional resource.
    type' :: Prelude.Maybe AdditionalResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'additionalResources_content' - The URLs for additional resources, either helpful resources or
-- improvement plans. Up to five additional URLs can be specified.
--
-- 'type'', 'additionalResources_type' - Type of additional resource.
newAdditionalResources ::
  AdditionalResources
newAdditionalResources =
  AdditionalResources'
    { content = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The URLs for additional resources, either helpful resources or
-- improvement plans. Up to five additional URLs can be specified.
additionalResources_content :: Lens.Lens' AdditionalResources (Prelude.Maybe [ChoiceContent])
additionalResources_content = Lens.lens (\AdditionalResources' {content} -> content) (\s@AdditionalResources' {} a -> s {content = a} :: AdditionalResources) Prelude.. Lens.mapping Lens.coerced

-- | Type of additional resource.
additionalResources_type :: Lens.Lens' AdditionalResources (Prelude.Maybe AdditionalResourceType)
additionalResources_type = Lens.lens (\AdditionalResources' {type'} -> type') (\s@AdditionalResources' {} a -> s {type' = a} :: AdditionalResources)

instance Data.FromJSON AdditionalResources where
  parseJSON =
    Data.withObject
      "AdditionalResources"
      ( \x ->
          AdditionalResources'
            Prelude.<$> (x Data..:? "Content" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AdditionalResources where
  hashWithSalt _salt AdditionalResources' {..} =
    _salt `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AdditionalResources where
  rnf AdditionalResources' {..} =
    Prelude.rnf content `Prelude.seq` Prelude.rnf type'
