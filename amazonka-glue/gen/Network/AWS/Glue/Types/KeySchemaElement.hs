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
-- Module      : Network.AWS.Glue.Types.KeySchemaElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.KeySchemaElement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A partition key pair consisting of a name and a type.
--
-- /See:/ 'newKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The name of a partition key.
    name :: Core.Text,
    -- | The type of a partition key.
    type' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'type''
  Core.Text ->
  KeySchemaElement
newKeySchemaElement pName_ pType_ =
  KeySchemaElement' {name = pName_, type' = pType_}

-- | The name of a partition key.
keySchemaElement_name :: Lens.Lens' KeySchemaElement Core.Text
keySchemaElement_name = Lens.lens (\KeySchemaElement' {name} -> name) (\s@KeySchemaElement' {} a -> s {name = a} :: KeySchemaElement)

-- | The type of a partition key.
keySchemaElement_type :: Lens.Lens' KeySchemaElement Core.Text
keySchemaElement_type = Lens.lens (\KeySchemaElement' {type'} -> type') (\s@KeySchemaElement' {} a -> s {type' = a} :: KeySchemaElement)

instance Core.FromJSON KeySchemaElement where
  parseJSON =
    Core.withObject
      "KeySchemaElement"
      ( \x ->
          KeySchemaElement'
            Core.<$> (x Core..: "Name") Core.<*> (x Core..: "Type")
      )

instance Core.Hashable KeySchemaElement

instance Core.NFData KeySchemaElement
