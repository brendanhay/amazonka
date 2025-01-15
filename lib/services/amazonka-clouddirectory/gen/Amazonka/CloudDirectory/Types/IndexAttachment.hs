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
-- Module      : Amazonka.CloudDirectory.Types.IndexAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.IndexAttachment where

import Amazonka.CloudDirectory.Types.AttributeKeyAndValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an index and an attached object.
--
-- /See:/ 'newIndexAttachment' smart constructor.
data IndexAttachment = IndexAttachment'
  { -- | The indexed attribute values.
    indexedAttributes :: Prelude.Maybe [AttributeKeyAndValue],
    -- | In response to ListIndex, the @ObjectIdentifier@ of the object attached
    -- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
    -- of the index attached to the object. This field will always contain the
    -- @ObjectIdentifier@ of the object on the opposite side of the attachment
    -- specified in the query.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexedAttributes', 'indexAttachment_indexedAttributes' - The indexed attribute values.
--
-- 'objectIdentifier', 'indexAttachment_objectIdentifier' - In response to ListIndex, the @ObjectIdentifier@ of the object attached
-- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
-- of the index attached to the object. This field will always contain the
-- @ObjectIdentifier@ of the object on the opposite side of the attachment
-- specified in the query.
newIndexAttachment ::
  IndexAttachment
newIndexAttachment =
  IndexAttachment'
    { indexedAttributes =
        Prelude.Nothing,
      objectIdentifier = Prelude.Nothing
    }

-- | The indexed attribute values.
indexAttachment_indexedAttributes :: Lens.Lens' IndexAttachment (Prelude.Maybe [AttributeKeyAndValue])
indexAttachment_indexedAttributes = Lens.lens (\IndexAttachment' {indexedAttributes} -> indexedAttributes) (\s@IndexAttachment' {} a -> s {indexedAttributes = a} :: IndexAttachment) Prelude.. Lens.mapping Lens.coerced

-- | In response to ListIndex, the @ObjectIdentifier@ of the object attached
-- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
-- of the index attached to the object. This field will always contain the
-- @ObjectIdentifier@ of the object on the opposite side of the attachment
-- specified in the query.
indexAttachment_objectIdentifier :: Lens.Lens' IndexAttachment (Prelude.Maybe Prelude.Text)
indexAttachment_objectIdentifier = Lens.lens (\IndexAttachment' {objectIdentifier} -> objectIdentifier) (\s@IndexAttachment' {} a -> s {objectIdentifier = a} :: IndexAttachment)

instance Data.FromJSON IndexAttachment where
  parseJSON =
    Data.withObject
      "IndexAttachment"
      ( \x ->
          IndexAttachment'
            Prelude.<$> ( x
                            Data..:? "IndexedAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ObjectIdentifier")
      )

instance Prelude.Hashable IndexAttachment where
  hashWithSalt _salt IndexAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` indexedAttributes
      `Prelude.hashWithSalt` objectIdentifier

instance Prelude.NFData IndexAttachment where
  rnf IndexAttachment' {..} =
    Prelude.rnf indexedAttributes `Prelude.seq`
      Prelude.rnf objectIdentifier
