{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteSlotTypeVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a slot type. To delete all versions of a slot type, use the 'DeleteSlotType' operation.
--
-- This operation requires permissions for the @lex:DeleteSlotTypeVersion@ action.
module Network.AWS.LexModels.DeleteSlotTypeVersion
  ( -- * Creating a request
    DeleteSlotTypeVersion (..),
    mkDeleteSlotTypeVersion,

    -- ** Request lenses
    dstvName,
    dstvVersion,

    -- * Destructuring the response
    DeleteSlotTypeVersionResponse (..),
    mkDeleteSlotTypeVersionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSlotTypeVersion' smart constructor.
data DeleteSlotTypeVersion = DeleteSlotTypeVersion'
  { -- | The name of the slot type.
    name :: Lude.Text,
    -- | The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
    version :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSlotTypeVersion' with the minimum fields required to make a request.
--
-- * 'name' - The name of the slot type.
-- * 'version' - The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
mkDeleteSlotTypeVersion ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  DeleteSlotTypeVersion
mkDeleteSlotTypeVersion pName_ pVersion_ =
  DeleteSlotTypeVersion' {name = pName_, version = pVersion_}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstvName :: Lens.Lens' DeleteSlotTypeVersion Lude.Text
dstvName = Lens.lens (name :: DeleteSlotTypeVersion -> Lude.Text) (\s a -> s {name = a} :: DeleteSlotTypeVersion)
{-# DEPRECATED dstvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstvVersion :: Lens.Lens' DeleteSlotTypeVersion Lude.Text
dstvVersion = Lens.lens (version :: DeleteSlotTypeVersion -> Lude.Text) (\s a -> s {version = a} :: DeleteSlotTypeVersion)
{-# DEPRECATED dstvVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeleteSlotTypeVersion where
  type Rs DeleteSlotTypeVersion = DeleteSlotTypeVersionResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteSlotTypeVersionResponse'

instance Lude.ToHeaders DeleteSlotTypeVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSlotTypeVersion where
  toPath DeleteSlotTypeVersion' {..} =
    Lude.mconcat
      ["/slottypes/", Lude.toBS name, "/version/", Lude.toBS version]

instance Lude.ToQuery DeleteSlotTypeVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSlotTypeVersionResponse' smart constructor.
data DeleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSlotTypeVersionResponse' with the minimum fields required to make a request.
mkDeleteSlotTypeVersionResponse ::
  DeleteSlotTypeVersionResponse
mkDeleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'
