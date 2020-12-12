{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.WriteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.WriteRequest
  ( WriteRequest (..),

    -- * Smart constructor
    mkWriteRequest,

    -- * Lenses
    wrDeleteRequest,
    wrPutRequest,
  )
where

import Network.AWS.DynamoDB.Types.DeleteRequest
import Network.AWS.DynamoDB.Types.PutRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an operation to perform - either @DeleteItem@ or @PutItem@ . You can only request one of these operations, not both, in a single @WriteRequest@ . If you do need to perform both of these operations, you need to provide two separate @WriteRequest@ objects.
--
-- /See:/ 'mkWriteRequest' smart constructor.
data WriteRequest = WriteRequest'
  { deleteRequest ::
      Lude.Maybe DeleteRequest,
    putRequest :: Lude.Maybe PutRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteRequest' with the minimum fields required to make a request.
--
-- * 'deleteRequest' - A request to perform a @DeleteItem@ operation.
-- * 'putRequest' - A request to perform a @PutItem@ operation.
mkWriteRequest ::
  WriteRequest
mkWriteRequest =
  WriteRequest'
    { deleteRequest = Lude.Nothing,
      putRequest = Lude.Nothing
    }

-- | A request to perform a @DeleteItem@ operation.
--
-- /Note:/ Consider using 'deleteRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrDeleteRequest :: Lens.Lens' WriteRequest (Lude.Maybe DeleteRequest)
wrDeleteRequest = Lens.lens (deleteRequest :: WriteRequest -> Lude.Maybe DeleteRequest) (\s a -> s {deleteRequest = a} :: WriteRequest)
{-# DEPRECATED wrDeleteRequest "Use generic-lens or generic-optics with 'deleteRequest' instead." #-}

-- | A request to perform a @PutItem@ operation.
--
-- /Note:/ Consider using 'putRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrPutRequest :: Lens.Lens' WriteRequest (Lude.Maybe PutRequest)
wrPutRequest = Lens.lens (putRequest :: WriteRequest -> Lude.Maybe PutRequest) (\s a -> s {putRequest = a} :: WriteRequest)
{-# DEPRECATED wrPutRequest "Use generic-lens or generic-optics with 'putRequest' instead." #-}

instance Lude.FromJSON WriteRequest where
  parseJSON =
    Lude.withObject
      "WriteRequest"
      ( \x ->
          WriteRequest'
            Lude.<$> (x Lude..:? "DeleteRequest") Lude.<*> (x Lude..:? "PutRequest")
      )

instance Lude.ToJSON WriteRequest where
  toJSON WriteRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteRequest" Lude..=) Lude.<$> deleteRequest,
            ("PutRequest" Lude..=) Lude.<$> putRequest
          ]
      )
