{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementResponse
  ( BatchStatementResponse (..),

    -- * Smart constructor
    mkBatchStatementResponse,

    -- * Lenses
    bError,
    bItem,
    bTableName,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.BatchStatementError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A PartiQL batch statement response..
--
-- /See:/ 'mkBatchStatementResponse' smart constructor.
data BatchStatementResponse = BatchStatementResponse'
  { -- | The error associated with a failed PartiQL batch statement.
    error :: Lude.Maybe BatchStatementError,
    -- | A DynamoDB item associated with a BatchStatementResponse
    item :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    -- | The table name associated with a failed PartiQL batch statement.
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStatementResponse' with the minimum fields required to make a request.
--
-- * 'error' - The error associated with a failed PartiQL batch statement.
-- * 'item' - A DynamoDB item associated with a BatchStatementResponse
-- * 'tableName' - The table name associated with a failed PartiQL batch statement.
mkBatchStatementResponse ::
  BatchStatementResponse
mkBatchStatementResponse =
  BatchStatementResponse'
    { error = Lude.Nothing,
      item = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | The error associated with a failed PartiQL batch statement.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bError :: Lens.Lens' BatchStatementResponse (Lude.Maybe BatchStatementError)
bError = Lens.lens (error :: BatchStatementResponse -> Lude.Maybe BatchStatementError) (\s a -> s {error = a} :: BatchStatementResponse)
{-# DEPRECATED bError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A DynamoDB item associated with a BatchStatementResponse
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bItem :: Lens.Lens' BatchStatementResponse (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
bItem = Lens.lens (item :: BatchStatementResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {item = a} :: BatchStatementResponse)
{-# DEPRECATED bItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The table name associated with a failed PartiQL batch statement.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTableName :: Lens.Lens' BatchStatementResponse (Lude.Maybe Lude.Text)
bTableName = Lens.lens (tableName :: BatchStatementResponse -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: BatchStatementResponse)
{-# DEPRECATED bTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON BatchStatementResponse where
  parseJSON =
    Lude.withObject
      "BatchStatementResponse"
      ( \x ->
          BatchStatementResponse'
            Lude.<$> (x Lude..:? "Error")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TableName")
      )
