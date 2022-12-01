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
-- Module      : Amazonka.Mobile.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an instance of an AWS resource associated with a
-- project.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { name :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    feature :: Prelude.Maybe Prelude.Text,
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'name', 'resource_name' - Undocumented member.
--
-- 'type'', 'resource_type' - Undocumented member.
--
-- 'arn', 'resource_arn' - Undocumented member.
--
-- 'feature', 'resource_feature' - Undocumented member.
--
-- 'attributes', 'resource_attributes' - Undocumented member.
newResource ::
  Resource
newResource =
  Resource'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = Prelude.Nothing,
      feature = Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | Undocumented member.
resource_name :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | Undocumented member.
resource_type :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

-- | Undocumented member.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | Undocumented member.
resource_feature :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_feature = Lens.lens (\Resource' {feature} -> feature) (\s@Resource' {} a -> s {feature = a} :: Resource)

-- | Undocumented member.
resource_attributes :: Lens.Lens' Resource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resource_attributes = Lens.lens (\Resource' {attributes} -> attributes) (\s@Resource' {} a -> s {attributes = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "feature")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf feature
      `Prelude.seq` Prelude.rnf attributes
