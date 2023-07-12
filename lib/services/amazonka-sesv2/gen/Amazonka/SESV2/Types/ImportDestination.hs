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
-- Module      : Amazonka.SESV2.Types.ImportDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ImportDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ContactListDestination
import Amazonka.SESV2.Types.SuppressionListDestination

-- | An object that contains details about the resource destination the
-- import job is going to target.
--
-- /See:/ 'newImportDestination' smart constructor.
data ImportDestination = ImportDestination'
  { -- | An object that contains the action of the import job towards a contact
    -- list.
    contactListDestination :: Prelude.Maybe ContactListDestination,
    -- | An object that contains the action of the import job towards suppression
    -- list.
    suppressionListDestination :: Prelude.Maybe SuppressionListDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListDestination', 'importDestination_contactListDestination' - An object that contains the action of the import job towards a contact
-- list.
--
-- 'suppressionListDestination', 'importDestination_suppressionListDestination' - An object that contains the action of the import job towards suppression
-- list.
newImportDestination ::
  ImportDestination
newImportDestination =
  ImportDestination'
    { contactListDestination =
        Prelude.Nothing,
      suppressionListDestination = Prelude.Nothing
    }

-- | An object that contains the action of the import job towards a contact
-- list.
importDestination_contactListDestination :: Lens.Lens' ImportDestination (Prelude.Maybe ContactListDestination)
importDestination_contactListDestination = Lens.lens (\ImportDestination' {contactListDestination} -> contactListDestination) (\s@ImportDestination' {} a -> s {contactListDestination = a} :: ImportDestination)

-- | An object that contains the action of the import job towards suppression
-- list.
importDestination_suppressionListDestination :: Lens.Lens' ImportDestination (Prelude.Maybe SuppressionListDestination)
importDestination_suppressionListDestination = Lens.lens (\ImportDestination' {suppressionListDestination} -> suppressionListDestination) (\s@ImportDestination' {} a -> s {suppressionListDestination = a} :: ImportDestination)

instance Data.FromJSON ImportDestination where
  parseJSON =
    Data.withObject
      "ImportDestination"
      ( \x ->
          ImportDestination'
            Prelude.<$> (x Data..:? "ContactListDestination")
            Prelude.<*> (x Data..:? "SuppressionListDestination")
      )

instance Prelude.Hashable ImportDestination where
  hashWithSalt _salt ImportDestination' {..} =
    _salt
      `Prelude.hashWithSalt` contactListDestination
      `Prelude.hashWithSalt` suppressionListDestination

instance Prelude.NFData ImportDestination where
  rnf ImportDestination' {..} =
    Prelude.rnf contactListDestination
      `Prelude.seq` Prelude.rnf suppressionListDestination

instance Data.ToJSON ImportDestination where
  toJSON ImportDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContactListDestination" Data..=)
              Prelude.<$> contactListDestination,
            ("SuppressionListDestination" Data..=)
              Prelude.<$> suppressionListDestination
          ]
      )
