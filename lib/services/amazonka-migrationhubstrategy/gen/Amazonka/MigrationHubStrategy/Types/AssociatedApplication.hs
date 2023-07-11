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
-- Module      : Amazonka.MigrationHubStrategy.Types.AssociatedApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AssociatedApplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object containing details about applications as defined in Application
-- Discovery Service.
--
-- /See:/ 'newAssociatedApplication' smart constructor.
data AssociatedApplication = AssociatedApplication'
  { -- | ID of the application as defined in Application Discovery Service.
    id :: Prelude.Maybe Prelude.Text,
    -- | Name of the application as defined in Application Discovery Service.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associatedApplication_id' - ID of the application as defined in Application Discovery Service.
--
-- 'name', 'associatedApplication_name' - Name of the application as defined in Application Discovery Service.
newAssociatedApplication ::
  AssociatedApplication
newAssociatedApplication =
  AssociatedApplication'
    { id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | ID of the application as defined in Application Discovery Service.
associatedApplication_id :: Lens.Lens' AssociatedApplication (Prelude.Maybe Prelude.Text)
associatedApplication_id = Lens.lens (\AssociatedApplication' {id} -> id) (\s@AssociatedApplication' {} a -> s {id = a} :: AssociatedApplication)

-- | Name of the application as defined in Application Discovery Service.
associatedApplication_name :: Lens.Lens' AssociatedApplication (Prelude.Maybe Prelude.Text)
associatedApplication_name = Lens.lens (\AssociatedApplication' {name} -> name) (\s@AssociatedApplication' {} a -> s {name = a} :: AssociatedApplication)

instance Data.FromJSON AssociatedApplication where
  parseJSON =
    Data.withObject
      "AssociatedApplication"
      ( \x ->
          AssociatedApplication'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable AssociatedApplication where
  hashWithSalt _salt AssociatedApplication' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData AssociatedApplication where
  rnf AssociatedApplication' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
