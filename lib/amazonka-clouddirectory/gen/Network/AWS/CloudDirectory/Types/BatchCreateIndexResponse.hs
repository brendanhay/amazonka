{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
  ( BatchCreateIndexResponse (..),

    -- * Smart constructor
    mkBatchCreateIndexResponse,

    -- * Lenses
    bciObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'CreateIndex' response operation.
--
-- /See:/ 'mkBatchCreateIndexResponse' smart constructor.
newtype BatchCreateIndexResponse = BatchCreateIndexResponse'
  { objectIdentifier ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchCreateIndexResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The @ObjectIdentifier@ of the index created by this operation.
mkBatchCreateIndexResponse ::
  BatchCreateIndexResponse
mkBatchCreateIndexResponse =
  BatchCreateIndexResponse' {objectIdentifier = Lude.Nothing}

-- | The @ObjectIdentifier@ of the index created by this operation.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciObjectIdentifier :: Lens.Lens' BatchCreateIndexResponse (Lude.Maybe Lude.Text)
bciObjectIdentifier = Lens.lens (objectIdentifier :: BatchCreateIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: BatchCreateIndexResponse)
{-# DEPRECATED bciObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Lude.FromJSON BatchCreateIndexResponse where
  parseJSON =
    Lude.withObject
      "BatchCreateIndexResponse"
      ( \x ->
          BatchCreateIndexResponse' Lude.<$> (x Lude..:? "ObjectIdentifier")
      )
