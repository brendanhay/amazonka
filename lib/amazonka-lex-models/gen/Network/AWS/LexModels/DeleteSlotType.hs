{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteSlotType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the slot type, including the @> LATEST@ version. To delete a specific version of the slot type, use the 'DeleteSlotTypeVersion' operation.
--
-- You can delete a version of a slot type only if it is not referenced. To delete a slot type that is referred to in one or more intents, you must remove those references first.
-- This operation requires permission for the @lex:DeleteSlotType@ action.
module Network.AWS.LexModels.DeleteSlotType
  ( -- * Creating a request
    DeleteSlotType (..),
    mkDeleteSlotType,

    -- ** Request lenses
    dstName,

    -- * Destructuring the response
    DeleteSlotTypeResponse (..),
    mkDeleteSlotTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSlotType' smart constructor.
newtype DeleteSlotType = DeleteSlotType'
  { -- | The name of the slot type. The name is case sensitive.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSlotType' with the minimum fields required to make a request.
--
-- * 'name' - The name of the slot type. The name is case sensitive.
mkDeleteSlotType ::
  -- | 'name'
  Lude.Text ->
  DeleteSlotType
mkDeleteSlotType pName_ = DeleteSlotType' {name = pName_}

-- | The name of the slot type. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstName :: Lens.Lens' DeleteSlotType Lude.Text
dstName = Lens.lens (name :: DeleteSlotType -> Lude.Text) (\s a -> s {name = a} :: DeleteSlotType)
{-# DEPRECATED dstName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteSlotType where
  type Rs DeleteSlotType = DeleteSlotTypeResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteSlotTypeResponse'

instance Lude.ToHeaders DeleteSlotType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSlotType where
  toPath DeleteSlotType' {..} =
    Lude.mconcat ["/slottypes/", Lude.toBS name]

instance Lude.ToQuery DeleteSlotType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSlotTypeResponse' smart constructor.
data DeleteSlotTypeResponse = DeleteSlotTypeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSlotTypeResponse' with the minimum fields required to make a request.
mkDeleteSlotTypeResponse ::
  DeleteSlotTypeResponse
mkDeleteSlotTypeResponse = DeleteSlotTypeResponse'
