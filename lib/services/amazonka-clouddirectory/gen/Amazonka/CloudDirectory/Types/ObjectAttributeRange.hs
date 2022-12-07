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
-- Module      : Amazonka.CloudDirectory.Types.ObjectAttributeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.ObjectAttributeRange where

import Amazonka.CloudDirectory.Types.AttributeKey
import Amazonka.CloudDirectory.Types.TypedAttributeValueRange
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of attributes.
--
-- /See:/ 'newObjectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { -- | The range of attribute values being selected.
    range :: Prelude.Maybe TypedAttributeValueRange,
    -- | The key of the attribute that the attribute range covers.
    attributeKey :: Prelude.Maybe AttributeKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable ObjectAttributeRange where
  hashWithSalt _salt ObjectAttributeRange' {..} =
    _salt `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` attributeKey

instance Prelude.NFData ObjectAttributeRange where
  rnf ObjectAttributeRange' {..} =
    Prelude.rnf range
      `Prelude.seq` Prelude.rnf attributeKey

instance Data.ToJSON ObjectAttributeRange where
  toJSON ObjectAttributeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Range" Data..=) Prelude.<$> range,
            ("AttributeKey" Data..=) Prelude.<$> attributeKey
          ]
      )
