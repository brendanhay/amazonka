{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
  ( DeleteGlobalSecondaryIndexAction (..),

    -- * Smart constructor
    mkDeleteGlobalSecondaryIndexAction,

    -- * Lenses
    dgsiaIndexName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a global secondary index to be deleted from an existing table.
--
-- /See:/ 'mkDeleteGlobalSecondaryIndexAction' smart constructor.
newtype DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
  { indexName ::
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

-- | Creates a value of 'DeleteGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index to be deleted.
mkDeleteGlobalSecondaryIndexAction ::
  -- | 'indexName'
  Lude.Text ->
  DeleteGlobalSecondaryIndexAction
mkDeleteGlobalSecondaryIndexAction pIndexName_ =
  DeleteGlobalSecondaryIndexAction' {indexName = pIndexName_}

-- | The name of the global secondary index to be deleted.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiaIndexName :: Lens.Lens' DeleteGlobalSecondaryIndexAction Lude.Text
dgsiaIndexName = Lens.lens (indexName :: DeleteGlobalSecondaryIndexAction -> Lude.Text) (\s a -> s {indexName = a} :: DeleteGlobalSecondaryIndexAction)
{-# DEPRECATED dgsiaIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON DeleteGlobalSecondaryIndexAction where
  toJSON DeleteGlobalSecondaryIndexAction' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("IndexName" Lude..= indexName)])
