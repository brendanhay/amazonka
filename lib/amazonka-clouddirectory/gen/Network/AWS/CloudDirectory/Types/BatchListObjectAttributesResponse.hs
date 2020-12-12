{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
  ( BatchListObjectAttributesResponse (..),

    -- * Smart constructor
    mkBatchListObjectAttributesResponse,

    -- * Lenses
    bNextToken,
    bAttributes,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListObjectAttributes' response operation.
--
-- /See:/ 'mkBatchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe
        [AttributeKeyAndValue]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListObjectAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
-- * 'nextToken' - The pagination token.
mkBatchListObjectAttributesResponse ::
  BatchListObjectAttributesResponse
mkBatchListObjectAttributesResponse =
  BatchListObjectAttributesResponse'
    { nextToken = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNextToken :: Lens.Lens' BatchListObjectAttributesResponse (Lude.Maybe Lude.Text)
bNextToken = Lens.lens (nextToken :: BatchListObjectAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectAttributesResponse)
{-# DEPRECATED bNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAttributes :: Lens.Lens' BatchListObjectAttributesResponse (Lude.Maybe [AttributeKeyAndValue])
bAttributes = Lens.lens (attributes :: BatchListObjectAttributesResponse -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {attributes = a} :: BatchListObjectAttributesResponse)
{-# DEPRECATED bAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON BatchListObjectAttributesResponse where
  parseJSON =
    Lude.withObject
      "BatchListObjectAttributesResponse"
      ( \x ->
          BatchListObjectAttributesResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
