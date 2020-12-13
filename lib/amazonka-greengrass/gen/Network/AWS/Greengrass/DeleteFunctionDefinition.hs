{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function definition.
module Network.AWS.Greengrass.DeleteFunctionDefinition
  ( -- * Creating a request
    DeleteFunctionDefinition (..),
    mkDeleteFunctionDefinition,

    -- ** Request lenses
    dfdFunctionDefinitionId,

    -- * Destructuring the response
    DeleteFunctionDefinitionResponse (..),
    mkDeleteFunctionDefinitionResponse,

    -- ** Response lenses
    dfdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFunctionDefinition' smart constructor.
newtype DeleteFunctionDefinition = DeleteFunctionDefinition'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionDefinition' with the minimum fields required to make a request.
--
-- * 'functionDefinitionId' - The ID of the Lambda function definition.
mkDeleteFunctionDefinition ::
  -- | 'functionDefinitionId'
  Lude.Text ->
  DeleteFunctionDefinition
mkDeleteFunctionDefinition pFunctionDefinitionId_ =
  DeleteFunctionDefinition'
    { functionDefinitionId =
        pFunctionDefinitionId_
    }

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdFunctionDefinitionId :: Lens.Lens' DeleteFunctionDefinition Lude.Text
dfdFunctionDefinitionId = Lens.lens (functionDefinitionId :: DeleteFunctionDefinition -> Lude.Text) (\s a -> s {functionDefinitionId = a} :: DeleteFunctionDefinition)
{-# DEPRECATED dfdFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteFunctionDefinition where
  type Rs DeleteFunctionDefinition = DeleteFunctionDefinitionResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteFunctionDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFunctionDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteFunctionDefinition where
  toPath DeleteFunctionDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/functions/",
        Lude.toBS functionDefinitionId
      ]

instance Lude.ToQuery DeleteFunctionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFunctionDefinitionResponse' smart constructor.
newtype DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFunctionDefinitionResponse
mkDeleteFunctionDefinitionResponse pResponseStatus_ =
  DeleteFunctionDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrsResponseStatus :: Lens.Lens' DeleteFunctionDefinitionResponse Lude.Int
dfdrsResponseStatus = Lens.lens (responseStatus :: DeleteFunctionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFunctionDefinitionResponse)
{-# DEPRECATED dfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
