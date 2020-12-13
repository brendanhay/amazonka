{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon ES domain.
module Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
  ( -- * Creating a request
    StartElasticsearchServiceSoftwareUpdate (..),
    mkStartElasticsearchServiceSoftwareUpdate,

    -- ** Request lenses
    sessuDomainName,

    -- * Destructuring the response
    StartElasticsearchServiceSoftwareUpdateResponse (..),
    mkStartElasticsearchServiceSoftwareUpdateResponse,

    -- ** Response lenses
    sessursServiceSoftwareOptions,
    sessursResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'StartElasticsearchServiceSoftwareUpdate' @ operation. Specifies the name of the Elasticsearch domain that you wish to schedule a service software update on.
--
-- /See:/ 'mkStartElasticsearchServiceSoftwareUpdate' smart constructor.
newtype StartElasticsearchServiceSoftwareUpdate = StartElasticsearchServiceSoftwareUpdate'
  { -- | The name of the domain that you want to update to the latest service software.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartElasticsearchServiceSoftwareUpdate' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to update to the latest service software.
mkStartElasticsearchServiceSoftwareUpdate ::
  -- | 'domainName'
  Lude.Text ->
  StartElasticsearchServiceSoftwareUpdate
mkStartElasticsearchServiceSoftwareUpdate pDomainName_ =
  StartElasticsearchServiceSoftwareUpdate'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to update to the latest service software.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sessuDomainName :: Lens.Lens' StartElasticsearchServiceSoftwareUpdate Lude.Text
sessuDomainName = Lens.lens (domainName :: StartElasticsearchServiceSoftwareUpdate -> Lude.Text) (\s a -> s {domainName = a} :: StartElasticsearchServiceSoftwareUpdate)
{-# DEPRECATED sessuDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest StartElasticsearchServiceSoftwareUpdate where
  type
    Rs StartElasticsearchServiceSoftwareUpdate =
      StartElasticsearchServiceSoftwareUpdateResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartElasticsearchServiceSoftwareUpdateResponse'
            Lude.<$> (x Lude..?> "ServiceSoftwareOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartElasticsearchServiceSoftwareUpdate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartElasticsearchServiceSoftwareUpdate where
  toJSON StartElasticsearchServiceSoftwareUpdate' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath StartElasticsearchServiceSoftwareUpdate where
  toPath = Lude.const "/2015-01-01/es/serviceSoftwareUpdate/start"

instance Lude.ToQuery StartElasticsearchServiceSoftwareUpdate where
  toQuery = Lude.const Lude.mempty

-- | The result of a @StartElasticsearchServiceSoftwareUpdate@ operation. Contains the status of the update.
--
-- /See:/ 'mkStartElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data StartElasticsearchServiceSoftwareUpdateResponse = StartElasticsearchServiceSoftwareUpdateResponse'
  { -- | The current status of the Elasticsearch service software update.
    serviceSoftwareOptions :: Lude.Maybe ServiceSoftwareOptions,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartElasticsearchServiceSoftwareUpdateResponse' with the minimum fields required to make a request.
--
-- * 'serviceSoftwareOptions' - The current status of the Elasticsearch service software update.
-- * 'responseStatus' - The response status code.
mkStartElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartElasticsearchServiceSoftwareUpdateResponse
mkStartElasticsearchServiceSoftwareUpdateResponse pResponseStatus_ =
  StartElasticsearchServiceSoftwareUpdateResponse'
    { serviceSoftwareOptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the Elasticsearch service software update.
--
-- /Note:/ Consider using 'serviceSoftwareOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sessursServiceSoftwareOptions :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse (Lude.Maybe ServiceSoftwareOptions)
sessursServiceSoftwareOptions = Lens.lens (serviceSoftwareOptions :: StartElasticsearchServiceSoftwareUpdateResponse -> Lude.Maybe ServiceSoftwareOptions) (\s a -> s {serviceSoftwareOptions = a} :: StartElasticsearchServiceSoftwareUpdateResponse)
{-# DEPRECATED sessursServiceSoftwareOptions "Use generic-lens or generic-optics with 'serviceSoftwareOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sessursResponseStatus :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse Lude.Int
sessursResponseStatus = Lens.lens (responseStatus :: StartElasticsearchServiceSoftwareUpdateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartElasticsearchServiceSoftwareUpdateResponse)
{-# DEPRECATED sessursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
