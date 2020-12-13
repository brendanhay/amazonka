{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.IndexAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.IndexAttachment
  ( IndexAttachment (..),

    -- * Smart constructor
    mkIndexAttachment,

    -- * Lenses
    iaIndexedAttributes,
    iaObjectIdentifier,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an index and an attached object.
--
-- /See:/ 'mkIndexAttachment' smart constructor.
data IndexAttachment = IndexAttachment'
  { -- | The indexed attribute values.
    indexedAttributes :: Lude.Maybe [AttributeKeyAndValue],
    -- | In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
    objectIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IndexAttachment' with the minimum fields required to make a request.
--
-- * 'indexedAttributes' - The indexed attribute values.
-- * 'objectIdentifier' - In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
mkIndexAttachment ::
  IndexAttachment
mkIndexAttachment =
  IndexAttachment'
    { indexedAttributes = Lude.Nothing,
      objectIdentifier = Lude.Nothing
    }

-- | The indexed attribute values.
--
-- /Note:/ Consider using 'indexedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaIndexedAttributes :: Lens.Lens' IndexAttachment (Lude.Maybe [AttributeKeyAndValue])
iaIndexedAttributes = Lens.lens (indexedAttributes :: IndexAttachment -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {indexedAttributes = a} :: IndexAttachment)
{-# DEPRECATED iaIndexedAttributes "Use generic-lens or generic-optics with 'indexedAttributes' instead." #-}

-- | In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaObjectIdentifier :: Lens.Lens' IndexAttachment (Lude.Maybe Lude.Text)
iaObjectIdentifier = Lens.lens (objectIdentifier :: IndexAttachment -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: IndexAttachment)
{-# DEPRECATED iaObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Lude.FromJSON IndexAttachment where
  parseJSON =
    Lude.withObject
      "IndexAttachment"
      ( \x ->
          IndexAttachment'
            Lude.<$> (x Lude..:? "IndexedAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ObjectIdentifier")
      )
