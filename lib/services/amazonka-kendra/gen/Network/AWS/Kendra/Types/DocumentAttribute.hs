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
-- Module      : Network.AWS.Kendra.Types.DocumentAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentAttribute where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DocumentAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A custom attribute value assigned to a document.
--
-- /See:/ 'newDocumentAttribute' smart constructor.
data DocumentAttribute = DocumentAttribute'
  { -- | The identifier for the attribute.
    key :: Prelude.Text,
    -- | The value of the attribute.
    value :: DocumentAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'documentAttribute_key' - The identifier for the attribute.
--
-- 'value', 'documentAttribute_value' - The value of the attribute.
newDocumentAttribute ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  DocumentAttributeValue ->
  DocumentAttribute
newDocumentAttribute pKey_ pValue_ =
  DocumentAttribute' {key = pKey_, value = pValue_}

-- | The identifier for the attribute.
documentAttribute_key :: Lens.Lens' DocumentAttribute Prelude.Text
documentAttribute_key = Lens.lens (\DocumentAttribute' {key} -> key) (\s@DocumentAttribute' {} a -> s {key = a} :: DocumentAttribute)

-- | The value of the attribute.
documentAttribute_value :: Lens.Lens' DocumentAttribute DocumentAttributeValue
documentAttribute_value = Lens.lens (\DocumentAttribute' {value} -> value) (\s@DocumentAttribute' {} a -> s {value = a} :: DocumentAttribute)

instance Core.FromJSON DocumentAttribute where
  parseJSON =
    Core.withObject
      "DocumentAttribute"
      ( \x ->
          DocumentAttribute'
            Prelude.<$> (x Core..: "Key") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable DocumentAttribute

instance Prelude.NFData DocumentAttribute

instance Core.ToJSON DocumentAttribute where
  toJSON DocumentAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Value" Core..= value)
          ]
      )
