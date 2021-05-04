{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.Types.Reference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Reference where

import Network.AWS.Connect.Types.ReferenceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A link that an agent selects to complete a given task. You can have up
-- to 4,096 UTF-8 bytes across all references for a contact.
--
-- /See:/ 'newReference' smart constructor.
data Reference = Reference'
  { -- | A formatted URL that displays to an agent in the Contact Control Panel
    -- (CCP)
    value :: Prelude.Text,
    -- | A valid URL.
    type' :: ReferenceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Reference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'reference_value' - A formatted URL that displays to an agent in the Contact Control Panel
-- (CCP)
--
-- 'type'', 'reference_type' - A valid URL.
newReference ::
  -- | 'value'
  Prelude.Text ->
  -- | 'type''
  ReferenceType ->
  Reference
newReference pValue_ pType_ =
  Reference' {value = pValue_, type' = pType_}

-- | A formatted URL that displays to an agent in the Contact Control Panel
-- (CCP)
reference_value :: Lens.Lens' Reference Prelude.Text
reference_value = Lens.lens (\Reference' {value} -> value) (\s@Reference' {} a -> s {value = a} :: Reference)

-- | A valid URL.
reference_type :: Lens.Lens' Reference ReferenceType
reference_type = Lens.lens (\Reference' {type'} -> type') (\s@Reference' {} a -> s {type' = a} :: Reference)

instance Prelude.Hashable Reference

instance Prelude.NFData Reference

instance Prelude.ToJSON Reference where
  toJSON Reference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Prelude..= value),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
