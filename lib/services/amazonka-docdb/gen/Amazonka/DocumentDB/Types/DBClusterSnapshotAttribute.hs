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
-- Module      : Amazonka.DocumentDB.Types.DBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBClusterSnapshotAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the name and values of a manual cluster snapshot attribute.
--
-- Manual cluster snapshot attributes are used to authorize other Amazon
-- Web Services accounts to restore a manual cluster snapshot.
--
-- /See:/ 'newDBClusterSnapshotAttribute' smart constructor.
data DBClusterSnapshotAttribute = DBClusterSnapshotAttribute'
  { -- | The values for the manual cluster snapshot attribute.
    --
    -- If the @AttributeName@ field is set to @restore@, then this element
    -- returns a list of IDs of the Amazon Web Services accounts that are
    -- authorized to copy or restore the manual cluster snapshot. If a value of
    -- @all@ is in the list, then the manual cluster snapshot is public and
    -- available for any Amazon Web Services account to copy or restore.
    attributeValues :: Prelude.Maybe [Prelude.Text],
    -- | The name of the manual cluster snapshot attribute.
    --
    -- The attribute named @restore@ refers to the list of Amazon Web Services
    -- accounts that have permission to copy or restore the manual cluster
    -- snapshot.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterSnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValues', 'dbClusterSnapshotAttribute_attributeValues' - The values for the manual cluster snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@, then this element
-- returns a list of IDs of the Amazon Web Services accounts that are
-- authorized to copy or restore the manual cluster snapshot. If a value of
-- @all@ is in the list, then the manual cluster snapshot is public and
-- available for any Amazon Web Services account to copy or restore.
--
-- 'attributeName', 'dbClusterSnapshotAttribute_attributeName' - The name of the manual cluster snapshot attribute.
--
-- The attribute named @restore@ refers to the list of Amazon Web Services
-- accounts that have permission to copy or restore the manual cluster
-- snapshot.
newDBClusterSnapshotAttribute ::
  DBClusterSnapshotAttribute
newDBClusterSnapshotAttribute =
  DBClusterSnapshotAttribute'
    { attributeValues =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The values for the manual cluster snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@, then this element
-- returns a list of IDs of the Amazon Web Services accounts that are
-- authorized to copy or restore the manual cluster snapshot. If a value of
-- @all@ is in the list, then the manual cluster snapshot is public and
-- available for any Amazon Web Services account to copy or restore.
dbClusterSnapshotAttribute_attributeValues :: Lens.Lens' DBClusterSnapshotAttribute (Prelude.Maybe [Prelude.Text])
dbClusterSnapshotAttribute_attributeValues = Lens.lens (\DBClusterSnapshotAttribute' {attributeValues} -> attributeValues) (\s@DBClusterSnapshotAttribute' {} a -> s {attributeValues = a} :: DBClusterSnapshotAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The name of the manual cluster snapshot attribute.
--
-- The attribute named @restore@ refers to the list of Amazon Web Services
-- accounts that have permission to copy or restore the manual cluster
-- snapshot.
dbClusterSnapshotAttribute_attributeName :: Lens.Lens' DBClusterSnapshotAttribute (Prelude.Maybe Prelude.Text)
dbClusterSnapshotAttribute_attributeName = Lens.lens (\DBClusterSnapshotAttribute' {attributeName} -> attributeName) (\s@DBClusterSnapshotAttribute' {} a -> s {attributeName = a} :: DBClusterSnapshotAttribute)

instance Core.FromXML DBClusterSnapshotAttribute where
  parseXML x =
    DBClusterSnapshotAttribute'
      Prelude.<$> ( x Core..@? "AttributeValues" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AttributeValue")
                  )
      Prelude.<*> (x Core..@? "AttributeName")

instance Prelude.Hashable DBClusterSnapshotAttribute where
  hashWithSalt _salt DBClusterSnapshotAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeValues
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData DBClusterSnapshotAttribute where
  rnf DBClusterSnapshotAttribute' {..} =
    Prelude.rnf attributeValues
      `Prelude.seq` Prelude.rnf attributeName
