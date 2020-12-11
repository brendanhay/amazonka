{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a logger definition.
module Network.AWS.Greengrass.UpdateLoggerDefinition
  ( -- * Creating a request
    UpdateLoggerDefinition (..),
    mkUpdateLoggerDefinition,

    -- ** Request lenses
    uldName,
    uldLoggerDefinitionId,

    -- * Destructuring the response
    UpdateLoggerDefinitionResponse (..),
    mkUpdateLoggerDefinitionResponse,

    -- ** Response lenses
    uldrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateLoggerDefinition' smart constructor.
data UpdateLoggerDefinition = UpdateLoggerDefinition'
  { name ::
      Lude.Maybe Lude.Text,
    loggerDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLoggerDefinition' with the minimum fields required to make a request.
--
-- * 'loggerDefinitionId' - The ID of the logger definition.
-- * 'name' - The name of the definition.
mkUpdateLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Lude.Text ->
  UpdateLoggerDefinition
mkUpdateLoggerDefinition pLoggerDefinitionId_ =
  UpdateLoggerDefinition'
    { name = Lude.Nothing,
      loggerDefinitionId = pLoggerDefinitionId_
    }

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uldName :: Lens.Lens' UpdateLoggerDefinition (Lude.Maybe Lude.Text)
uldName = Lens.lens (name :: UpdateLoggerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateLoggerDefinition)
{-# DEPRECATED uldName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uldLoggerDefinitionId :: Lens.Lens' UpdateLoggerDefinition Lude.Text
uldLoggerDefinitionId = Lens.lens (loggerDefinitionId :: UpdateLoggerDefinition -> Lude.Text) (\s a -> s {loggerDefinitionId = a} :: UpdateLoggerDefinition)
{-# DEPRECATED uldLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

instance Lude.AWSRequest UpdateLoggerDefinition where
  type Rs UpdateLoggerDefinition = UpdateLoggerDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateLoggerDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateLoggerDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateLoggerDefinition where
  toJSON UpdateLoggerDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateLoggerDefinition where
  toPath UpdateLoggerDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/loggers/", Lude.toBS loggerDefinitionId]

instance Lude.ToQuery UpdateLoggerDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateLoggerDefinitionResponse' smart constructor.
newtype UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse'
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

-- | Creates a value of 'UpdateLoggerDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateLoggerDefinitionResponse
mkUpdateLoggerDefinitionResponse pResponseStatus_ =
  UpdateLoggerDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uldrsResponseStatus :: Lens.Lens' UpdateLoggerDefinitionResponse Lude.Int
uldrsResponseStatus = Lens.lens (responseStatus :: UpdateLoggerDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateLoggerDefinitionResponse)
{-# DEPRECATED uldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
