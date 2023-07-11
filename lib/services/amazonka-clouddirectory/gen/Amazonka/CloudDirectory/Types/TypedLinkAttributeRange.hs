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
-- Module      : Amazonka.CloudDirectory.Types.TypedLinkAttributeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedLinkAttributeRange where

import Amazonka.CloudDirectory.Types.TypedAttributeValueRange
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the range of attributes that are used by a specified filter.
--
-- /See:/ 'newTypedLinkAttributeRange' smart constructor.
data TypedLinkAttributeRange = TypedLinkAttributeRange'
  { -- | The unique name of the typed link attribute.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The range of attribute values that are being selected.
    range :: TypedAttributeValueRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkAttributeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'typedLinkAttributeRange_attributeName' - The unique name of the typed link attribute.
--
-- 'range', 'typedLinkAttributeRange_range' - The range of attribute values that are being selected.
newTypedLinkAttributeRange ::
  -- | 'range'
  TypedAttributeValueRange ->
  TypedLinkAttributeRange
newTypedLinkAttributeRange pRange_ =
  TypedLinkAttributeRange'
    { attributeName =
        Prelude.Nothing,
      range = pRange_
    }

-- | The unique name of the typed link attribute.
typedLinkAttributeRange_attributeName :: Lens.Lens' TypedLinkAttributeRange (Prelude.Maybe Prelude.Text)
typedLinkAttributeRange_attributeName = Lens.lens (\TypedLinkAttributeRange' {attributeName} -> attributeName) (\s@TypedLinkAttributeRange' {} a -> s {attributeName = a} :: TypedLinkAttributeRange)

-- | The range of attribute values that are being selected.
typedLinkAttributeRange_range :: Lens.Lens' TypedLinkAttributeRange TypedAttributeValueRange
typedLinkAttributeRange_range = Lens.lens (\TypedLinkAttributeRange' {range} -> range) (\s@TypedLinkAttributeRange' {} a -> s {range = a} :: TypedLinkAttributeRange)

instance Prelude.Hashable TypedLinkAttributeRange where
  hashWithSalt _salt TypedLinkAttributeRange' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` range

instance Prelude.NFData TypedLinkAttributeRange where
  rnf TypedLinkAttributeRange' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf range

instance Data.ToJSON TypedLinkAttributeRange where
  toJSON TypedLinkAttributeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeName" Data..=) Prelude.<$> attributeName,
            Prelude.Just ("Range" Data..= range)
          ]
      )
