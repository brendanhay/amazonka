{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteQueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Route 53 doesn't delete any logs that are already in CloudWatch Logs.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> .
module Network.AWS.Route53.DeleteQueryLoggingConfig
  ( -- * Creating a request
    DeleteQueryLoggingConfig (..),
    mkDeleteQueryLoggingConfig,

    -- ** Request lenses
    dqlcId,

    -- * Destructuring the response
    DeleteQueryLoggingConfigResponse (..),
    mkDeleteQueryLoggingConfigResponse,

    -- ** Response lenses
    dqlcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | /See:/ 'mkDeleteQueryLoggingConfig' smart constructor.
newtype DeleteQueryLoggingConfig = DeleteQueryLoggingConfig'
  { -- | The ID of the configuration that you want to delete.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueryLoggingConfig' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the configuration that you want to delete.
mkDeleteQueryLoggingConfig ::
  -- | 'id'
  Lude.Text ->
  DeleteQueryLoggingConfig
mkDeleteQueryLoggingConfig pId_ =
  DeleteQueryLoggingConfig' {id = pId_}

-- | The ID of the configuration that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqlcId :: Lens.Lens' DeleteQueryLoggingConfig Lude.Text
dqlcId = Lens.lens (id :: DeleteQueryLoggingConfig -> Lude.Text) (\s a -> s {id = a} :: DeleteQueryLoggingConfig)
{-# DEPRECATED dqlcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteQueryLoggingConfig where
  type Rs DeleteQueryLoggingConfig = DeleteQueryLoggingConfigResponse
  request = Req.delete route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteQueryLoggingConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteQueryLoggingConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteQueryLoggingConfig where
  toPath DeleteQueryLoggingConfig' {..} =
    Lude.mconcat ["/2013-04-01/queryloggingconfig/", Lude.toBS id]

instance Lude.ToQuery DeleteQueryLoggingConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteQueryLoggingConfigResponse' smart constructor.
newtype DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueryLoggingConfigResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteQueryLoggingConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteQueryLoggingConfigResponse
mkDeleteQueryLoggingConfigResponse pResponseStatus_ =
  DeleteQueryLoggingConfigResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqlcrsResponseStatus :: Lens.Lens' DeleteQueryLoggingConfigResponse Lude.Int
dqlcrsResponseStatus = Lens.lens (responseStatus :: DeleteQueryLoggingConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteQueryLoggingConfigResponse)
{-# DEPRECATED dqlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
