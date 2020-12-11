{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connector definition.
module Network.AWS.Greengrass.UpdateConnectorDefinition
  ( -- * Creating a request
    UpdateConnectorDefinition (..),
    mkUpdateConnectorDefinition,

    -- ** Request lenses
    uName,
    uConnectorDefinitionId,

    -- * Destructuring the response
    UpdateConnectorDefinitionResponse (..),
    mkUpdateConnectorDefinitionResponse,

    -- ** Response lenses
    ucdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateConnectorDefinition' smart constructor.
data UpdateConnectorDefinition = UpdateConnectorDefinition'
  { name ::
      Lude.Maybe Lude.Text,
    connectorDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConnectorDefinition' with the minimum fields required to make a request.
--
-- * 'connectorDefinitionId' - The ID of the connector definition.
-- * 'name' - The name of the definition.
mkUpdateConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Lude.Text ->
  UpdateConnectorDefinition
mkUpdateConnectorDefinition pConnectorDefinitionId_ =
  UpdateConnectorDefinition'
    { name = Lude.Nothing,
      connectorDefinitionId = pConnectorDefinitionId_
    }

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateConnectorDefinition (Lude.Maybe Lude.Text)
uName = Lens.lens (name :: UpdateConnectorDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateConnectorDefinition)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConnectorDefinitionId :: Lens.Lens' UpdateConnectorDefinition Lude.Text
uConnectorDefinitionId = Lens.lens (connectorDefinitionId :: UpdateConnectorDefinition -> Lude.Text) (\s a -> s {connectorDefinitionId = a} :: UpdateConnectorDefinition)
{-# DEPRECATED uConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

instance Lude.AWSRequest UpdateConnectorDefinition where
  type
    Rs UpdateConnectorDefinition =
      UpdateConnectorDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateConnectorDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConnectorDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConnectorDefinition where
  toJSON UpdateConnectorDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateConnectorDefinition where
  toPath UpdateConnectorDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/connectors/",
        Lude.toBS connectorDefinitionId
      ]

instance Lude.ToQuery UpdateConnectorDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConnectorDefinitionResponse' smart constructor.
newtype UpdateConnectorDefinitionResponse = UpdateConnectorDefinitionResponse'
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

-- | Creates a value of 'UpdateConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConnectorDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConnectorDefinitionResponse
mkUpdateConnectorDefinitionResponse pResponseStatus_ =
  UpdateConnectorDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdrsResponseStatus :: Lens.Lens' UpdateConnectorDefinitionResponse Lude.Int
ucdrsResponseStatus = Lens.lens (responseStatus :: UpdateConnectorDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConnectorDefinitionResponse)
{-# DEPRECATED ucdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
