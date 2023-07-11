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
-- Module      : Amazonka.Glue.Types.Datatype
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Datatype where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure representing the datatype of the value.
--
-- /See:/ 'newDatatype' smart constructor.
data Datatype = Datatype'
  { -- | The datatype of the value.
    id :: Prelude.Text,
    -- | A label assigned to the datatype.
    label :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Datatype' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'datatype_id' - The datatype of the value.
--
-- 'label', 'datatype_label' - A label assigned to the datatype.
newDatatype ::
  -- | 'id'
  Prelude.Text ->
  -- | 'label'
  Prelude.Text ->
  Datatype
newDatatype pId_ pLabel_ =
  Datatype' {id = pId_, label = pLabel_}

-- | The datatype of the value.
datatype_id :: Lens.Lens' Datatype Prelude.Text
datatype_id = Lens.lens (\Datatype' {id} -> id) (\s@Datatype' {} a -> s {id = a} :: Datatype)

-- | A label assigned to the datatype.
datatype_label :: Lens.Lens' Datatype Prelude.Text
datatype_label = Lens.lens (\Datatype' {label} -> label) (\s@Datatype' {} a -> s {label = a} :: Datatype)

instance Data.FromJSON Datatype where
  parseJSON =
    Data.withObject
      "Datatype"
      ( \x ->
          Datatype'
            Prelude.<$> (x Data..: "Id")
            Prelude.<*> (x Data..: "Label")
      )

instance Prelude.Hashable Datatype where
  hashWithSalt _salt Datatype' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` label

instance Prelude.NFData Datatype where
  rnf Datatype' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf label

instance Data.ToJSON Datatype where
  toJSON Datatype' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Label" Data..= label)
          ]
      )
