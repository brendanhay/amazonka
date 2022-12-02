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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ResourceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details related to the resource.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | The value of the tag.
    tagValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagValue', 'resourceDetails_tagValue' - The value of the tag.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails' {tagValue = Prelude.Nothing}

-- | The value of the tag.
resourceDetails_tagValue :: Lens.Lens' ResourceDetails (Prelude.Maybe Prelude.Text)
resourceDetails_tagValue = Lens.lens (\ResourceDetails' {tagValue} -> tagValue) (\s@ResourceDetails' {} a -> s {tagValue = a} :: ResourceDetails)

instance Data.FromJSON ResourceDetails where
  parseJSON =
    Data.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails' Prelude.<$> (x Data..:? "tagValue")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt `Prelude.hashWithSalt` tagValue

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} = Prelude.rnf tagValue
