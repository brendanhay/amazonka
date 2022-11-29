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
-- Module      : Amazonka.ConnectCases.Types.FieldIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Object for unique identifier of a field.
--
-- /See:/ 'newFieldIdentifier' smart constructor.
data FieldIdentifier = FieldIdentifier'
  { -- | Unique identifier of a field.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'fieldIdentifier_id' - Unique identifier of a field.
newFieldIdentifier ::
  -- | 'id'
  Prelude.Text ->
  FieldIdentifier
newFieldIdentifier pId_ = FieldIdentifier' {id = pId_}

-- | Unique identifier of a field.
fieldIdentifier_id :: Lens.Lens' FieldIdentifier Prelude.Text
fieldIdentifier_id = Lens.lens (\FieldIdentifier' {id} -> id) (\s@FieldIdentifier' {} a -> s {id = a} :: FieldIdentifier)

instance Core.FromJSON FieldIdentifier where
  parseJSON =
    Core.withObject
      "FieldIdentifier"
      ( \x ->
          FieldIdentifier' Prelude.<$> (x Core..: "id")
      )

instance Prelude.Hashable FieldIdentifier where
  hashWithSalt _salt FieldIdentifier' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData FieldIdentifier where
  rnf FieldIdentifier' {..} = Prelude.rnf id

instance Core.ToJSON FieldIdentifier where
  toJSON FieldIdentifier' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])
