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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.TagQueryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.TagQueryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The definition of @tagQuery@. Specifies which resources are associated
-- with an application.
--
-- /See:/ 'newTagQueryConfiguration' smart constructor.
data TagQueryConfiguration = TagQueryConfiguration'
  { -- | Condition in the IAM policy that associates resources to an application.
    tagKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagQueryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'tagQueryConfiguration_tagKey' - Condition in the IAM policy that associates resources to an application.
newTagQueryConfiguration ::
  TagQueryConfiguration
newTagQueryConfiguration =
  TagQueryConfiguration' {tagKey = Prelude.Nothing}

-- | Condition in the IAM policy that associates resources to an application.
tagQueryConfiguration_tagKey :: Lens.Lens' TagQueryConfiguration (Prelude.Maybe Prelude.Text)
tagQueryConfiguration_tagKey = Lens.lens (\TagQueryConfiguration' {tagKey} -> tagKey) (\s@TagQueryConfiguration' {} a -> s {tagKey = a} :: TagQueryConfiguration)

instance Core.FromJSON TagQueryConfiguration where
  parseJSON =
    Core.withObject
      "TagQueryConfiguration"
      ( \x ->
          TagQueryConfiguration'
            Prelude.<$> (x Core..:? "tagKey")
      )

instance Prelude.Hashable TagQueryConfiguration where
  hashWithSalt _salt TagQueryConfiguration' {..} =
    _salt `Prelude.hashWithSalt` tagKey

instance Prelude.NFData TagQueryConfiguration where
  rnf TagQueryConfiguration' {..} = Prelude.rnf tagKey

instance Core.ToJSON TagQueryConfiguration where
  toJSON TagQueryConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("tagKey" Core..=) Prelude.<$> tagKey]
      )
