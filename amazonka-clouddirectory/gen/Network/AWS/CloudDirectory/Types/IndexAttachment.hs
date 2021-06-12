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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an index and an attached object.
--
-- /See:/ 'newIndexAttachment' smart constructor.
data IndexAttachment = IndexAttachment'
  { -- | In response to ListIndex, the @ObjectIdentifier@ of the object attached
    -- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
    -- of the index attached to the object. This field will always contain the
    -- @ObjectIdentifier@ of the object on the opposite side of the attachment
    -- specified in the query.
    objectIdentifier :: Core.Maybe Core.Text,
    -- | The indexed attribute values.
    indexedAttributes :: Core.Maybe [AttributeKeyAndValue]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { objectIdentifier = Core.Nothing,
      indexedAttributes = Core.Nothing
    }

-- | In response to ListIndex, the @ObjectIdentifier@ of the object attached
-- to the index. In response to ListAttachedIndices, the @ObjectIdentifier@
-- of the index attached to the object. This field will always contain the
-- @ObjectIdentifier@ of the object on the opposite side of the attachment
-- specified in the query.
indexAttachment_objectIdentifier :: Lens.Lens' IndexAttachment (Core.Maybe Core.Text)
indexAttachment_objectIdentifier = Lens.lens (\IndexAttachment' {objectIdentifier} -> objectIdentifier) (\s@IndexAttachment' {} a -> s {objectIdentifier = a} :: IndexAttachment)

-- | The indexed attribute values.
indexAttachment_indexedAttributes :: Lens.Lens' IndexAttachment (Core.Maybe [AttributeKeyAndValue])
indexAttachment_indexedAttributes = Lens.lens (\IndexAttachment' {indexedAttributes} -> indexedAttributes) (\s@IndexAttachment' {} a -> s {indexedAttributes = a} :: IndexAttachment) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON IndexAttachment where
  parseJSON =
    Core.withObject
      "IndexAttachment"
      ( \x ->
          IndexAttachment'
            Core.<$> (x Core..:? "ObjectIdentifier")
            Core.<*> ( x Core..:? "IndexedAttributes"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable IndexAttachment

instance Core.NFData IndexAttachment
