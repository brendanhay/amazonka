{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetQueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified configuration for DNS query logging.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> and <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> .
module Network.AWS.Route53.GetQueryLoggingConfig
  ( -- * Creating a request
    GetQueryLoggingConfig (..),
    mkGetQueryLoggingConfig,

    -- ** Request lenses
    gqlcId,

    -- * Destructuring the response
    GetQueryLoggingConfigResponse (..),
    mkGetQueryLoggingConfigResponse,

    -- ** Response lenses
    gqlcrsQueryLoggingConfig,
    gqlcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | /See:/ 'mkGetQueryLoggingConfig' smart constructor.
newtype GetQueryLoggingConfig = GetQueryLoggingConfig'
  { -- | The ID of the configuration for DNS query logging that you want to get information about.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueryLoggingConfig' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the configuration for DNS query logging that you want to get information about.
mkGetQueryLoggingConfig ::
  -- | 'id'
  Lude.Text ->
  GetQueryLoggingConfig
mkGetQueryLoggingConfig pId_ = GetQueryLoggingConfig' {id = pId_}

-- | The ID of the configuration for DNS query logging that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqlcId :: Lens.Lens' GetQueryLoggingConfig Lude.Text
gqlcId = Lens.lens (id :: GetQueryLoggingConfig -> Lude.Text) (\s a -> s {id = a} :: GetQueryLoggingConfig)
{-# DEPRECATED gqlcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetQueryLoggingConfig where
  type Rs GetQueryLoggingConfig = GetQueryLoggingConfigResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetQueryLoggingConfigResponse'
            Lude.<$> (x Lude..@ "QueryLoggingConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQueryLoggingConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetQueryLoggingConfig where
  toPath GetQueryLoggingConfig' {..} =
    Lude.mconcat ["/2013-04-01/queryloggingconfig/", Lude.toBS id]

instance Lude.ToQuery GetQueryLoggingConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetQueryLoggingConfigResponse' smart constructor.
data GetQueryLoggingConfigResponse = GetQueryLoggingConfigResponse'
  { -- | A complex type that contains information about the query logging configuration that you specified in a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig> request.
    queryLoggingConfig :: QueryLoggingConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueryLoggingConfigResponse' with the minimum fields required to make a request.
--
-- * 'queryLoggingConfig' - A complex type that contains information about the query logging configuration that you specified in a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig> request.
-- * 'responseStatus' - The response status code.
mkGetQueryLoggingConfigResponse ::
  -- | 'queryLoggingConfig'
  QueryLoggingConfig ->
  -- | 'responseStatus'
  Lude.Int ->
  GetQueryLoggingConfigResponse
mkGetQueryLoggingConfigResponse
  pQueryLoggingConfig_
  pResponseStatus_ =
    GetQueryLoggingConfigResponse'
      { queryLoggingConfig =
          pQueryLoggingConfig_,
        responseStatus = pResponseStatus_
      }

-- | A complex type that contains information about the query logging configuration that you specified in a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig> request.
--
-- /Note:/ Consider using 'queryLoggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqlcrsQueryLoggingConfig :: Lens.Lens' GetQueryLoggingConfigResponse QueryLoggingConfig
gqlcrsQueryLoggingConfig = Lens.lens (queryLoggingConfig :: GetQueryLoggingConfigResponse -> QueryLoggingConfig) (\s a -> s {queryLoggingConfig = a} :: GetQueryLoggingConfigResponse)
{-# DEPRECATED gqlcrsQueryLoggingConfig "Use generic-lens or generic-optics with 'queryLoggingConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqlcrsResponseStatus :: Lens.Lens' GetQueryLoggingConfigResponse Lude.Int
gqlcrsResponseStatus = Lens.lens (responseStatus :: GetQueryLoggingConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQueryLoggingConfigResponse)
{-# DEPRECATED gqlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
