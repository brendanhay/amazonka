{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function definition.
module Network.AWS.Greengrass.UpdateFunctionDefinition
  ( -- * Creating a request
    UpdateFunctionDefinition (..),
    mkUpdateFunctionDefinition,

    -- ** Request lenses
    ufdName,
    ufdFunctionDefinitionId,

    -- * Destructuring the response
    UpdateFunctionDefinitionResponse (..),
    mkUpdateFunctionDefinitionResponse,

    -- ** Response lenses
    ufdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFunctionDefinition' smart constructor.
data UpdateFunctionDefinition = UpdateFunctionDefinition'
  { name ::
      Lude.Maybe Lude.Text,
    functionDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunctionDefinition' with the minimum fields required to make a request.
--
-- * 'functionDefinitionId' - The ID of the Lambda function definition.
-- * 'name' - The name of the definition.
mkUpdateFunctionDefinition ::
  -- | 'functionDefinitionId'
  Lude.Text ->
  UpdateFunctionDefinition
mkUpdateFunctionDefinition pFunctionDefinitionId_ =
  UpdateFunctionDefinition'
    { name = Lude.Nothing,
      functionDefinitionId = pFunctionDefinitionId_
    }

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdName :: Lens.Lens' UpdateFunctionDefinition (Lude.Maybe Lude.Text)
ufdName = Lens.lens (name :: UpdateFunctionDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateFunctionDefinition)
{-# DEPRECATED ufdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdFunctionDefinitionId :: Lens.Lens' UpdateFunctionDefinition Lude.Text
ufdFunctionDefinitionId = Lens.lens (functionDefinitionId :: UpdateFunctionDefinition -> Lude.Text) (\s a -> s {functionDefinitionId = a} :: UpdateFunctionDefinition)
{-# DEPRECATED ufdFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

instance Lude.AWSRequest UpdateFunctionDefinition where
  type Rs UpdateFunctionDefinition = UpdateFunctionDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateFunctionDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFunctionDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFunctionDefinition where
  toJSON UpdateFunctionDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateFunctionDefinition where
  toPath UpdateFunctionDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/functions/",
        Lude.toBS functionDefinitionId
      ]

instance Lude.ToQuery UpdateFunctionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFunctionDefinitionResponse' smart constructor.
newtype UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse'
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

-- | Creates a value of 'UpdateFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFunctionDefinitionResponse
mkUpdateFunctionDefinitionResponse pResponseStatus_ =
  UpdateFunctionDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdrsResponseStatus :: Lens.Lens' UpdateFunctionDefinitionResponse Lude.Int
ufdrsResponseStatus = Lens.lens (responseStatus :: UpdateFunctionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFunctionDefinitionResponse)
{-# DEPRECATED ufdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
