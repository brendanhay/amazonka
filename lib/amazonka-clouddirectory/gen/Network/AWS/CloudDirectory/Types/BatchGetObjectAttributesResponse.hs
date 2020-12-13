{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
  ( BatchGetObjectAttributesResponse (..),

    -- * Smart constructor
    mkBatchGetObjectAttributesResponse,

    -- * Lenses
    bgoaAttributes,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'GetObjectAttributes' response operation.
--
-- /See:/ 'mkBatchGetObjectAttributesResponse' smart constructor.
newtype BatchGetObjectAttributesResponse = BatchGetObjectAttributesResponse'
  { -- | The attribute values that are associated with an object.
    attributes :: Lude.Maybe [AttributeKeyAndValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetObjectAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attribute values that are associated with an object.
mkBatchGetObjectAttributesResponse ::
  BatchGetObjectAttributesResponse
mkBatchGetObjectAttributesResponse =
  BatchGetObjectAttributesResponse' {attributes = Lude.Nothing}

-- | The attribute values that are associated with an object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoaAttributes :: Lens.Lens' BatchGetObjectAttributesResponse (Lude.Maybe [AttributeKeyAndValue])
bgoaAttributes = Lens.lens (attributes :: BatchGetObjectAttributesResponse -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {attributes = a} :: BatchGetObjectAttributesResponse)
{-# DEPRECATED bgoaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON BatchGetObjectAttributesResponse where
  parseJSON =
    Lude.withObject
      "BatchGetObjectAttributesResponse"
      ( \x ->
          BatchGetObjectAttributesResponse'
            Lude.<$> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
