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
-- Module      : Amazonka.RDS.Types.DBSnapshotAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBSnapshotAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the name and values of a manual DB snapshot attribute
--
-- Manual DB snapshot attributes are used to authorize other Amazon Web
-- Services accounts to restore a manual DB snapshot. For more information,
-- see the @ModifyDBSnapshotAttribute@ API.
--
-- /See:/ 'newDBSnapshotAttribute' smart constructor.
data DBSnapshotAttribute = DBSnapshotAttribute'
  { -- | The value or values for the manual DB snapshot attribute.
    --
    -- If the @AttributeName@ field is set to @restore@, then this element
    -- returns a list of IDs of the Amazon Web Services accounts that are
    -- authorized to copy or restore the manual DB snapshot. If a value of
    -- @all@ is in the list, then the manual DB snapshot is public and
    -- available for any Amazon Web Services account to copy or restore.
    attributeValues :: Prelude.Maybe [Prelude.Text],
    -- | The name of the manual DB snapshot attribute.
    --
    -- The attribute named @restore@ refers to the list of Amazon Web Services
    -- accounts that have permission to copy or restore the manual DB cluster
    -- snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API
    -- action.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValues', 'dbSnapshotAttribute_attributeValues' - The value or values for the manual DB snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@, then this element
-- returns a list of IDs of the Amazon Web Services accounts that are
-- authorized to copy or restore the manual DB snapshot. If a value of
-- @all@ is in the list, then the manual DB snapshot is public and
-- available for any Amazon Web Services account to copy or restore.
--
-- 'attributeName', 'dbSnapshotAttribute_attributeName' - The name of the manual DB snapshot attribute.
--
-- The attribute named @restore@ refers to the list of Amazon Web Services
-- accounts that have permission to copy or restore the manual DB cluster
-- snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API
-- action.
newDBSnapshotAttribute ::
  DBSnapshotAttribute
newDBSnapshotAttribute =
  DBSnapshotAttribute'
    { attributeValues =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The value or values for the manual DB snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@, then this element
-- returns a list of IDs of the Amazon Web Services accounts that are
-- authorized to copy or restore the manual DB snapshot. If a value of
-- @all@ is in the list, then the manual DB snapshot is public and
-- available for any Amazon Web Services account to copy or restore.
dbSnapshotAttribute_attributeValues :: Lens.Lens' DBSnapshotAttribute (Prelude.Maybe [Prelude.Text])
dbSnapshotAttribute_attributeValues = Lens.lens (\DBSnapshotAttribute' {attributeValues} -> attributeValues) (\s@DBSnapshotAttribute' {} a -> s {attributeValues = a} :: DBSnapshotAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The name of the manual DB snapshot attribute.
--
-- The attribute named @restore@ refers to the list of Amazon Web Services
-- accounts that have permission to copy or restore the manual DB cluster
-- snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API
-- action.
dbSnapshotAttribute_attributeName :: Lens.Lens' DBSnapshotAttribute (Prelude.Maybe Prelude.Text)
dbSnapshotAttribute_attributeName = Lens.lens (\DBSnapshotAttribute' {attributeName} -> attributeName) (\s@DBSnapshotAttribute' {} a -> s {attributeName = a} :: DBSnapshotAttribute)

instance Data.FromXML DBSnapshotAttribute where
  parseXML x =
    DBSnapshotAttribute'
      Prelude.<$> ( x Data..@? "AttributeValues" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AttributeValue")
                  )
      Prelude.<*> (x Data..@? "AttributeName")

instance Prelude.Hashable DBSnapshotAttribute where
  hashWithSalt _salt DBSnapshotAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeValues
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData DBSnapshotAttribute where
  rnf DBSnapshotAttribute' {..} =
    Prelude.rnf attributeValues
      `Prelude.seq` Prelude.rnf attributeName
