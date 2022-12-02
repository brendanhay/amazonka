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
-- Module      : Amazonka.ConnectCases.Types.FieldItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for field related information.
--
-- /See:/ 'newFieldItem' smart constructor.
data FieldItem = FieldItem'
  { -- | Unique identifier of a field.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'fieldItem_id' - Unique identifier of a field.
newFieldItem ::
  -- | 'id'
  Prelude.Text ->
  FieldItem
newFieldItem pId_ = FieldItem' {id = pId_}

-- | Unique identifier of a field.
fieldItem_id :: Lens.Lens' FieldItem Prelude.Text
fieldItem_id = Lens.lens (\FieldItem' {id} -> id) (\s@FieldItem' {} a -> s {id = a} :: FieldItem)

instance Data.FromJSON FieldItem where
  parseJSON =
    Data.withObject
      "FieldItem"
      (\x -> FieldItem' Prelude.<$> (x Data..: "id"))

instance Prelude.Hashable FieldItem where
  hashWithSalt _salt FieldItem' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData FieldItem where
  rnf FieldItem' {..} = Prelude.rnf id

instance Data.ToJSON FieldItem where
  toJSON FieldItem' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])
