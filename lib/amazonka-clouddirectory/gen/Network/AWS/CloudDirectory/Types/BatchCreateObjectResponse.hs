{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
  ( BatchCreateObjectResponse (..),

    -- * Smart constructor
    mkBatchCreateObjectResponse,

    -- * Lenses
    bcoObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'CreateObject' response operation.
--
-- /See:/ 'mkBatchCreateObjectResponse' smart constructor.
newtype BatchCreateObjectResponse = BatchCreateObjectResponse'
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

-- | Creates a value of 'BatchCreateObjectResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The ID that is associated with the object.
mkBatchCreateObjectResponse ::
  BatchCreateObjectResponse
mkBatchCreateObjectResponse =
  BatchCreateObjectResponse' {objectIdentifier = Lude.Nothing}

-- | The ID that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoObjectIdentifier :: Lens.Lens' BatchCreateObjectResponse (Lude.Maybe Lude.Text)
bcoObjectIdentifier = Lens.lens (objectIdentifier :: BatchCreateObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: BatchCreateObjectResponse)
{-# DEPRECATED bcoObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Lude.FromJSON BatchCreateObjectResponse where
  parseJSON =
    Lude.withObject
      "BatchCreateObjectResponse"
      ( \x ->
          BatchCreateObjectResponse'
            Lude.<$> (x Lude..:? "ObjectIdentifier")
      )
