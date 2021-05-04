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
-- Module      : Network.AWS.CloudDirectory.Types.IndexAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.IndexAttachment where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an index and an attached object.
--
-- /See:/ 'newIndexAttachment' smart constructor.
data IndexAttachment = IndexAttachment'
  { -- | In response to ListIndex, the @ObjectIdentifier@ of the object attached
    -- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
    -- of the index attached to the object. This field will always contain the
    -- @ObjectIdentifier@ of the object on the opposite side of the attachment
    -- specified in the query.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The indexed attribute values.
    indexedAttributes :: Prelude.Maybe [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IndexAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'indexAttachment_objectIdentifier' - In response to ListIndex, the @ObjectIdentifier@ of the object attached
-- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
-- of the index attached to the object. This field will always contain the
-- @ObjectIdentifier@ of the object on the opposite side of the attachment
-- specified in the query.
--
-- 'indexedAttributes', 'indexAttachment_indexedAttributes' - The indexed attribute values.
newIndexAttachment ::
  IndexAttachment
newIndexAttachment =
  IndexAttachment'
    { objectIdentifier =
        Prelude.Nothing,
      indexedAttributes = Prelude.Nothing
    }

-- | In response to ListIndex, the @ObjectIdentifier@ of the object attached
-- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
-- of the index attached to the object. This field will always contain the
-- @ObjectIdentifier@ of the object on the opposite side of the attachment
-- specified in the query.
indexAttachment_objectIdentifier :: Lens.Lens' IndexAttachment (Prelude.Maybe Prelude.Text)
indexAttachment_objectIdentifier = Lens.lens (\IndexAttachment' {objectIdentifier} -> objectIdentifier) (\s@IndexAttachment' {} a -> s {objectIdentifier = a} :: IndexAttachment)

-- | The indexed attribute values.
indexAttachment_indexedAttributes :: Lens.Lens' IndexAttachment (Prelude.Maybe [AttributeKeyAndValue])
indexAttachment_indexedAttributes = Lens.lens (\IndexAttachment' {indexedAttributes} -> indexedAttributes) (\s@IndexAttachment' {} a -> s {indexedAttributes = a} :: IndexAttachment) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON IndexAttachment where
  parseJSON =
    Prelude.withObject
      "IndexAttachment"
      ( \x ->
          IndexAttachment'
            Prelude.<$> (x Prelude..:? "ObjectIdentifier")
            Prelude.<*> ( x Prelude..:? "IndexedAttributes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable IndexAttachment

instance Prelude.NFData IndexAttachment
