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
-- Module      : Amazonka.AuditManager.Types.ServiceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ServiceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata that\'s associated with the Amazon Web Service.
--
-- /See:/ 'newServiceMetadata' smart constructor.
data ServiceMetadata = ServiceMetadata'
  { -- | The category that the Amazon Web Service belongs to, such as compute,
    -- storage, or database.
    category :: Prelude.Maybe Prelude.Text,
    -- | The description of the Amazon Web Service.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the Amazon Web Service.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Service.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'serviceMetadata_category' - The category that the Amazon Web Service belongs to, such as compute,
-- storage, or database.
--
-- 'description', 'serviceMetadata_description' - The description of the Amazon Web Service.
--
-- 'displayName', 'serviceMetadata_displayName' - The display name of the Amazon Web Service.
--
-- 'name', 'serviceMetadata_name' - The name of the Amazon Web Service.
newServiceMetadata ::
  ServiceMetadata
newServiceMetadata =
  ServiceMetadata'
    { category = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The category that the Amazon Web Service belongs to, such as compute,
-- storage, or database.
serviceMetadata_category :: Lens.Lens' ServiceMetadata (Prelude.Maybe Prelude.Text)
serviceMetadata_category = Lens.lens (\ServiceMetadata' {category} -> category) (\s@ServiceMetadata' {} a -> s {category = a} :: ServiceMetadata)

-- | The description of the Amazon Web Service.
serviceMetadata_description :: Lens.Lens' ServiceMetadata (Prelude.Maybe Prelude.Text)
serviceMetadata_description = Lens.lens (\ServiceMetadata' {description} -> description) (\s@ServiceMetadata' {} a -> s {description = a} :: ServiceMetadata)

-- | The display name of the Amazon Web Service.
serviceMetadata_displayName :: Lens.Lens' ServiceMetadata (Prelude.Maybe Prelude.Text)
serviceMetadata_displayName = Lens.lens (\ServiceMetadata' {displayName} -> displayName) (\s@ServiceMetadata' {} a -> s {displayName = a} :: ServiceMetadata)

-- | The name of the Amazon Web Service.
serviceMetadata_name :: Lens.Lens' ServiceMetadata (Prelude.Maybe Prelude.Text)
serviceMetadata_name = Lens.lens (\ServiceMetadata' {name} -> name) (\s@ServiceMetadata' {} a -> s {name = a} :: ServiceMetadata)

instance Data.FromJSON ServiceMetadata where
  parseJSON =
    Data.withObject
      "ServiceMetadata"
      ( \x ->
          ServiceMetadata'
            Prelude.<$> (x Data..:? "category")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ServiceMetadata where
  hashWithSalt _salt ServiceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` name

instance Prelude.NFData ServiceMetadata where
  rnf ServiceMetadata' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf name
