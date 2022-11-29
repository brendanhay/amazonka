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
-- Module      : Amazonka.Connect.Types.Reference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Reference where

import Amazonka.Connect.Types.ReferenceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Well-formed data on a contact, used by agents to complete a contact
-- request. You can have up to 4,096 UTF-8 bytes across all references for
-- a contact.
--
-- /See:/ 'newReference' smart constructor.
data Reference = Reference'
  { -- | A valid value for the reference. For example, for a URL reference, a
    -- formatted URL that is displayed to an agent in the Contact Control Panel
    -- (CCP).
    value :: Prelude.Text,
    -- | The type of the reference. @DATE@ must be of type Epoch timestamp.
    type' :: ReferenceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Reference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'reference_value' - A valid value for the reference. For example, for a URL reference, a
-- formatted URL that is displayed to an agent in the Contact Control Panel
-- (CCP).
--
-- 'type'', 'reference_type' - The type of the reference. @DATE@ must be of type Epoch timestamp.
newReference ::
  -- | 'value'
  Prelude.Text ->
  -- | 'type''
  ReferenceType ->
  Reference
newReference pValue_ pType_ =
  Reference' {value = pValue_, type' = pType_}

-- | A valid value for the reference. For example, for a URL reference, a
-- formatted URL that is displayed to an agent in the Contact Control Panel
-- (CCP).
reference_value :: Lens.Lens' Reference Prelude.Text
reference_value = Lens.lens (\Reference' {value} -> value) (\s@Reference' {} a -> s {value = a} :: Reference)

-- | The type of the reference. @DATE@ must be of type Epoch timestamp.
reference_type :: Lens.Lens' Reference ReferenceType
reference_type = Lens.lens (\Reference' {type'} -> type') (\s@Reference' {} a -> s {type' = a} :: Reference)

instance Prelude.Hashable Reference where
  hashWithSalt _salt Reference' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Reference where
  rnf Reference' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON Reference where
  toJSON Reference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Core..= value),
            Prelude.Just ("Type" Core..= type')
          ]
      )
