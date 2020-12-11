{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow definition.
module Network.AWS.SageMaker.DeleteFlowDefinition
  ( -- * Creating a request
    DeleteFlowDefinition (..),
    mkDeleteFlowDefinition,

    -- ** Request lenses
    dfdFlowDefinitionName,

    -- * Destructuring the response
    DeleteFlowDefinitionResponse (..),
    mkDeleteFlowDefinitionResponse,

    -- ** Response lenses
    dfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteFlowDefinition' smart constructor.
newtype DeleteFlowDefinition = DeleteFlowDefinition'
  { flowDefinitionName ::
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

-- | Creates a value of 'DeleteFlowDefinition' with the minimum fields required to make a request.
--
-- * 'flowDefinitionName' - The name of the flow definition you are deleting.
mkDeleteFlowDefinition ::
  -- | 'flowDefinitionName'
  Lude.Text ->
  DeleteFlowDefinition
mkDeleteFlowDefinition pFlowDefinitionName_ =
  DeleteFlowDefinition' {flowDefinitionName = pFlowDefinitionName_}

-- | The name of the flow definition you are deleting.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdFlowDefinitionName :: Lens.Lens' DeleteFlowDefinition Lude.Text
dfdFlowDefinitionName = Lens.lens (flowDefinitionName :: DeleteFlowDefinition -> Lude.Text) (\s a -> s {flowDefinitionName = a} :: DeleteFlowDefinition)
{-# DEPRECATED dfdFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

instance Lude.AWSRequest DeleteFlowDefinition where
  type Rs DeleteFlowDefinition = DeleteFlowDefinitionResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteFlowDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFlowDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteFlowDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteFlowDefinition where
  toJSON DeleteFlowDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("FlowDefinitionName" Lude..= flowDefinitionName)]
      )

instance Lude.ToPath DeleteFlowDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFlowDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFlowDefinitionResponse' smart constructor.
newtype DeleteFlowDefinitionResponse = DeleteFlowDefinitionResponse'
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

-- | Creates a value of 'DeleteFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteFlowDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFlowDefinitionResponse
mkDeleteFlowDefinitionResponse pResponseStatus_ =
  DeleteFlowDefinitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrsResponseStatus :: Lens.Lens' DeleteFlowDefinitionResponse Lude.Int
dfdrsResponseStatus = Lens.lens (responseStatus :: DeleteFlowDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFlowDefinitionResponse)
{-# DEPRECATED dfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
