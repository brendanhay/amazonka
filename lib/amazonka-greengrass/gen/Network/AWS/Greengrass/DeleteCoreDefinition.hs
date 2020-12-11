{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Network.AWS.Greengrass.DeleteCoreDefinition
  ( -- * Creating a request
    DeleteCoreDefinition (..),
    mkDeleteCoreDefinition,

    -- ** Request lenses
    dcdCoreDefinitionId,

    -- * Destructuring the response
    DeleteCoreDefinitionResponse (..),
    mkDeleteCoreDefinitionResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCoreDefinition' smart constructor.
newtype DeleteCoreDefinition = DeleteCoreDefinition'
  { coreDefinitionId ::
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

-- | Creates a value of 'DeleteCoreDefinition' with the minimum fields required to make a request.
--
-- * 'coreDefinitionId' - The ID of the core definition.
mkDeleteCoreDefinition ::
  -- | 'coreDefinitionId'
  Lude.Text ->
  DeleteCoreDefinition
mkDeleteCoreDefinition pCoreDefinitionId_ =
  DeleteCoreDefinition' {coreDefinitionId = pCoreDefinitionId_}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdCoreDefinitionId :: Lens.Lens' DeleteCoreDefinition Lude.Text
dcdCoreDefinitionId = Lens.lens (coreDefinitionId :: DeleteCoreDefinition -> Lude.Text) (\s a -> s {coreDefinitionId = a} :: DeleteCoreDefinition)
{-# DEPRECATED dcdCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteCoreDefinition where
  type Rs DeleteCoreDefinition = DeleteCoreDefinitionResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCoreDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCoreDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteCoreDefinition where
  toPath DeleteCoreDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/cores/", Lude.toBS coreDefinitionId]

instance Lude.ToQuery DeleteCoreDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCoreDefinitionResponse' smart constructor.
newtype DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCoreDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCoreDefinitionResponse
mkDeleteCoreDefinitionResponse pResponseStatus_ =
  DeleteCoreDefinitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteCoreDefinitionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteCoreDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCoreDefinitionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
