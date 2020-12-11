{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connector definition.
module Network.AWS.Greengrass.DeleteConnectorDefinition
  ( -- * Creating a request
    DeleteConnectorDefinition (..),
    mkDeleteConnectorDefinition,

    -- ** Request lenses
    dcdConnectorDefinitionId,

    -- * Destructuring the response
    DeleteConnectorDefinitionResponse (..),
    mkDeleteConnectorDefinitionResponse,

    -- ** Response lenses
    dcdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteConnectorDefinition' smart constructor.
newtype DeleteConnectorDefinition = DeleteConnectorDefinition'
  { connectorDefinitionId ::
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

-- | Creates a value of 'DeleteConnectorDefinition' with the minimum fields required to make a request.
--
-- * 'connectorDefinitionId' - The ID of the connector definition.
mkDeleteConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Lude.Text ->
  DeleteConnectorDefinition
mkDeleteConnectorDefinition pConnectorDefinitionId_ =
  DeleteConnectorDefinition'
    { connectorDefinitionId =
        pConnectorDefinitionId_
    }

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdConnectorDefinitionId :: Lens.Lens' DeleteConnectorDefinition Lude.Text
dcdConnectorDefinitionId = Lens.lens (connectorDefinitionId :: DeleteConnectorDefinition -> Lude.Text) (\s a -> s {connectorDefinitionId = a} :: DeleteConnectorDefinition)
{-# DEPRECATED dcdConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteConnectorDefinition where
  type
    Rs DeleteConnectorDefinition =
      DeleteConnectorDefinitionResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteConnectorDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConnectorDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteConnectorDefinition where
  toPath DeleteConnectorDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/connectors/",
        Lude.toBS connectorDefinitionId
      ]

instance Lude.ToQuery DeleteConnectorDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConnectorDefinitionResponse' smart constructor.
newtype DeleteConnectorDefinitionResponse = DeleteConnectorDefinitionResponse'
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

-- | Creates a value of 'DeleteConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConnectorDefinitionResponse
mkDeleteConnectorDefinitionResponse pResponseStatus_ =
  DeleteConnectorDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdrsResponseStatus :: Lens.Lens' DeleteConnectorDefinitionResponse Lude.Int
dcdrsResponseStatus = Lens.lens (responseStatus :: DeleteConnectorDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConnectorDefinitionResponse)
{-# DEPRECATED dcdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
