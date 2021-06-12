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
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange where

import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies the range of attributes that are used by a specified filter.
--
-- /See:/ 'newTypedLinkAttributeRange' smart constructor.
data TypedLinkAttributeRange = TypedLinkAttributeRange'
  { -- | The unique name of the typed link attribute.
    attributeName :: Core.Maybe Core.Text,
    -- | The range of attribute values that are being selected.
    range :: TypedAttributeValueRange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      range = pRange_
    }

-- | The unique name of the typed link attribute.
typedLinkAttributeRange_attributeName :: Lens.Lens' TypedLinkAttributeRange (Core.Maybe Core.Text)
typedLinkAttributeRange_attributeName = Lens.lens (\TypedLinkAttributeRange' {attributeName} -> attributeName) (\s@TypedLinkAttributeRange' {} a -> s {attributeName = a} :: TypedLinkAttributeRange)

-- | The range of attribute values that are being selected.
typedLinkAttributeRange_range :: Lens.Lens' TypedLinkAttributeRange TypedAttributeValueRange
typedLinkAttributeRange_range = Lens.lens (\TypedLinkAttributeRange' {range} -> range) (\s@TypedLinkAttributeRange' {} a -> s {range = a} :: TypedLinkAttributeRange)

instance Core.Hashable TypedLinkAttributeRange

instance Core.NFData TypedLinkAttributeRange

instance Core.ToJSON TypedLinkAttributeRange where
  toJSON TypedLinkAttributeRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeName" Core..=) Core.<$> attributeName,
            Core.Just ("Range" Core..= range)
          ]
      )
