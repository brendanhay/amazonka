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
-- Module      : Amazonka.Glue.Types.KeySchemaElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.KeySchemaElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A partition key pair consisting of a name and a type.
--
-- /See:/ 'newKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The name of a partition key.
    name :: Prelude.Text,
    -- | The type of a partition key.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeySchemaElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'keySchemaElement_name' - The name of a partition key.
--
-- 'type'', 'keySchemaElement_type' - The type of a partition key.
newKeySchemaElement ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  KeySchemaElement
newKeySchemaElement pName_ pType_ =
  KeySchemaElement' {name = pName_, type' = pType_}

-- | The name of a partition key.
keySchemaElement_name :: Lens.Lens' KeySchemaElement Prelude.Text
keySchemaElement_name = Lens.lens (\KeySchemaElement' {name} -> name) (\s@KeySchemaElement' {} a -> s {name = a} :: KeySchemaElement)

-- | The type of a partition key.
keySchemaElement_type :: Lens.Lens' KeySchemaElement Prelude.Text
keySchemaElement_type = Lens.lens (\KeySchemaElement' {type'} -> type') (\s@KeySchemaElement' {} a -> s {type' = a} :: KeySchemaElement)

instance Data.FromJSON KeySchemaElement where
  parseJSON =
    Data.withObject
      "KeySchemaElement"
      ( \x ->
          KeySchemaElement'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable KeySchemaElement where
  hashWithSalt _salt KeySchemaElement' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData KeySchemaElement where
  rnf KeySchemaElement' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
