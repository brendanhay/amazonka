{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
  ( BatchGetLinkAttributesResponse (..),

    -- * Smart constructor
    mkBatchGetLinkAttributesResponse,

    -- * Lenses
    bglaAttributes,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'GetLinkAttributes' response operation.
--
-- /See:/ 'mkBatchGetLinkAttributesResponse' smart constructor.
newtype BatchGetLinkAttributesResponse = BatchGetLinkAttributesResponse'
  { attributes ::
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
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetLinkAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes that are associated with the typed link.
mkBatchGetLinkAttributesResponse ::
  BatchGetLinkAttributesResponse
mkBatchGetLinkAttributesResponse =
  BatchGetLinkAttributesResponse' {attributes = Lude.Nothing}

-- | The attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bglaAttributes :: Lens.Lens' BatchGetLinkAttributesResponse (Lude.Maybe [AttributeKeyAndValue])
bglaAttributes = Lens.lens (attributes :: BatchGetLinkAttributesResponse -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {attributes = a} :: BatchGetLinkAttributesResponse)
{-# DEPRECATED bglaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON BatchGetLinkAttributesResponse where
  parseJSON =
    Lude.withObject
      "BatchGetLinkAttributesResponse"
      ( \x ->
          BatchGetLinkAttributesResponse'
            Lude.<$> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
