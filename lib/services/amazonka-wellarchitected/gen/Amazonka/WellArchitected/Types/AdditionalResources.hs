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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | Type of additional resource.
    type' :: Prelude.Maybe AdditionalResourceType,
    -- | The URLs for additional resources, either helpful resources or
    -- improvement plans. Up to five additional URLs can be specified.
    content :: Prelude.Maybe [ChoiceContent]
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
-- 'type'', 'additionalResources_type' - Type of additional resource.
--
-- 'content', 'additionalResources_content' - The URLs for additional resources, either helpful resources or
-- improvement plans. Up to five additional URLs can be specified.
newAdditionalResources ::
  AdditionalResources
newAdditionalResources =
  AdditionalResources'
    { type' = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | Type of additional resource.
additionalResources_type :: Lens.Lens' AdditionalResources (Prelude.Maybe AdditionalResourceType)
additionalResources_type = Lens.lens (\AdditionalResources' {type'} -> type') (\s@AdditionalResources' {} a -> s {type' = a} :: AdditionalResources)

-- | The URLs for additional resources, either helpful resources or
-- improvement plans. Up to five additional URLs can be specified.
additionalResources_content :: Lens.Lens' AdditionalResources (Prelude.Maybe [ChoiceContent])
additionalResources_content = Lens.lens (\AdditionalResources' {content} -> content) (\s@AdditionalResources' {} a -> s {content = a} :: AdditionalResources) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AdditionalResources where
  parseJSON =
    Data.withObject
      "AdditionalResources"
      ( \x ->
          AdditionalResources'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Content" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AdditionalResources where
  hashWithSalt _salt AdditionalResources' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` content

instance Prelude.NFData AdditionalResources where
  rnf AdditionalResources' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf content
