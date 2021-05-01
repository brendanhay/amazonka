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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeRange where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A range of attributes.
--
-- /See:/ 'newObjectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { -- | The range of attribute values being selected.
    range :: Prelude.Maybe TypedAttributeValueRange,
    -- | The key of the attribute that the attribute range covers.
    attributeKey :: Prelude.Maybe AttributeKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { range = Prelude.Nothing,
      attributeKey = Prelude.Nothing
    }

-- | The range of attribute values being selected.
objectAttributeRange_range :: Lens.Lens' ObjectAttributeRange (Prelude.Maybe TypedAttributeValueRange)
objectAttributeRange_range = Lens.lens (\ObjectAttributeRange' {range} -> range) (\s@ObjectAttributeRange' {} a -> s {range = a} :: ObjectAttributeRange)

-- | The key of the attribute that the attribute range covers.
objectAttributeRange_attributeKey :: Lens.Lens' ObjectAttributeRange (Prelude.Maybe AttributeKey)
objectAttributeRange_attributeKey = Lens.lens (\ObjectAttributeRange' {attributeKey} -> attributeKey) (\s@ObjectAttributeRange' {} a -> s {attributeKey = a} :: ObjectAttributeRange)

instance Prelude.Hashable ObjectAttributeRange

instance Prelude.NFData ObjectAttributeRange

instance Prelude.ToJSON ObjectAttributeRange where
  toJSON ObjectAttributeRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Range" Prelude..=) Prelude.<$> range,
            ("AttributeKey" Prelude..=)
              Prelude.<$> attributeKey
          ]
      )
