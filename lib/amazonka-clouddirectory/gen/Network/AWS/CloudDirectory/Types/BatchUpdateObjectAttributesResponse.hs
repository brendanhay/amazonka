{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
  ( BatchUpdateObjectAttributesResponse (..),

    -- * Smart constructor
    mkBatchUpdateObjectAttributesResponse,

    -- * Lenses
    buoaObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchUpdate@ response operation.
--
-- /See:/ 'mkBatchUpdateObjectAttributesResponse' smart constructor.
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse'
  { objectIdentifier ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdateObjectAttributesResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - ID that is associated with the object.
mkBatchUpdateObjectAttributesResponse ::
  BatchUpdateObjectAttributesResponse
mkBatchUpdateObjectAttributesResponse =
  BatchUpdateObjectAttributesResponse'
    { objectIdentifier =
        Lude.Nothing
    }

-- | ID that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buoaObjectIdentifier :: Lens.Lens' BatchUpdateObjectAttributesResponse (Lude.Maybe Lude.Text)
buoaObjectIdentifier = Lens.lens (objectIdentifier :: BatchUpdateObjectAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: BatchUpdateObjectAttributesResponse)
{-# DEPRECATED buoaObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Lude.FromJSON BatchUpdateObjectAttributesResponse where
  parseJSON =
    Lude.withObject
      "BatchUpdateObjectAttributesResponse"
      ( \x ->
          BatchUpdateObjectAttributesResponse'
            Lude.<$> (x Lude..:? "ObjectIdentifier")
      )
