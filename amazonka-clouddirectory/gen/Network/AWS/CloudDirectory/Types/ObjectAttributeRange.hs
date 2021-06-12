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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeRange where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A range of attributes.
--
-- /See:/ 'newObjectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { -- | The range of attribute values being selected.
    range :: Core.Maybe TypedAttributeValueRange,
    -- | The key of the attribute that the attribute range covers.
    attributeKey :: Core.Maybe AttributeKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ObjectAttributeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'range', 'objectAttributeRange_range' - The range of attribute values being selected.
--
-- 'attributeKey', 'objectAttributeRange_attributeKey' - The key of the attribute that the attribute range covers.
newObjectAttributeRange ::
  ObjectAttributeRange
newObjectAttributeRange =
  ObjectAttributeRange'
    { range = Core.Nothing,
      attributeKey = Core.Nothing
    }

-- | The range of attribute values being selected.
objectAttributeRange_range :: Lens.Lens' ObjectAttributeRange (Core.Maybe TypedAttributeValueRange)
objectAttributeRange_range = Lens.lens (\ObjectAttributeRange' {range} -> range) (\s@ObjectAttributeRange' {} a -> s {range = a} :: ObjectAttributeRange)

-- | The key of the attribute that the attribute range covers.
objectAttributeRange_attributeKey :: Lens.Lens' ObjectAttributeRange (Core.Maybe AttributeKey)
objectAttributeRange_attributeKey = Lens.lens (\ObjectAttributeRange' {attributeKey} -> attributeKey) (\s@ObjectAttributeRange' {} a -> s {attributeKey = a} :: ObjectAttributeRange)

instance Core.Hashable ObjectAttributeRange

instance Core.NFData ObjectAttributeRange

instance Core.ToJSON ObjectAttributeRange where
  toJSON ObjectAttributeRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Range" Core..=) Core.<$> range,
            ("AttributeKey" Core..=) Core.<$> attributeKey
          ]
      )
