{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled service software update for an Amazon ES domain. You can only perform this operation before the @AutomatedUpdateDate@ and when the @UpdateStatus@ is in the @PENDING_UPDATE@ state.
module Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
  ( -- * Creating a request
    CancelElasticsearchServiceSoftwareUpdate (..),
    mkCancelElasticsearchServiceSoftwareUpdate,

    -- ** Request lenses
    cessuDomainName,

    -- * Destructuring the response
    CancelElasticsearchServiceSoftwareUpdateResponse (..),
    mkCancelElasticsearchServiceSoftwareUpdateResponse,

    -- ** Response lenses
    cessursServiceSoftwareOptions,
    cessursResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'CancelElasticsearchServiceSoftwareUpdate' @ operation. Specifies the name of the Elasticsearch domain that you wish to cancel a service software update on.
--
-- /See:/ 'mkCancelElasticsearchServiceSoftwareUpdate' smart constructor.
newtype CancelElasticsearchServiceSoftwareUpdate = CancelElasticsearchServiceSoftwareUpdate'
  { domainName ::
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

-- | Creates a value of 'CancelElasticsearchServiceSoftwareUpdate' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to stop the latest service software update on.
mkCancelElasticsearchServiceSoftwareUpdate ::
  -- | 'domainName'
  Lude.Text ->
  CancelElasticsearchServiceSoftwareUpdate
mkCancelElasticsearchServiceSoftwareUpdate pDomainName_ =
  CancelElasticsearchServiceSoftwareUpdate'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to stop the latest service software update on.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cessuDomainName :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdate Lude.Text
cessuDomainName = Lens.lens (domainName :: CancelElasticsearchServiceSoftwareUpdate -> Lude.Text) (\s a -> s {domainName = a} :: CancelElasticsearchServiceSoftwareUpdate)
{-# DEPRECATED cessuDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CancelElasticsearchServiceSoftwareUpdate where
  type
    Rs CancelElasticsearchServiceSoftwareUpdate =
      CancelElasticsearchServiceSoftwareUpdateResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelElasticsearchServiceSoftwareUpdateResponse'
            Lude.<$> (x Lude..?> "ServiceSoftwareOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelElasticsearchServiceSoftwareUpdate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CancelElasticsearchServiceSoftwareUpdate where
  toJSON CancelElasticsearchServiceSoftwareUpdate' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath CancelElasticsearchServiceSoftwareUpdate where
  toPath = Lude.const "/2015-01-01/es/serviceSoftwareUpdate/cancel"

instance Lude.ToQuery CancelElasticsearchServiceSoftwareUpdate where
  toQuery = Lude.const Lude.mempty

-- | The result of a @CancelElasticsearchServiceSoftwareUpdate@ operation. Contains the status of the update.
--
-- /See:/ 'mkCancelElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data CancelElasticsearchServiceSoftwareUpdateResponse = CancelElasticsearchServiceSoftwareUpdateResponse'
  { serviceSoftwareOptions ::
      Lude.Maybe
        ServiceSoftwareOptions,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CancelElasticsearchServiceSoftwareUpdateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'serviceSoftwareOptions' - The current status of the Elasticsearch service software update.
mkCancelElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelElasticsearchServiceSoftwareUpdateResponse
mkCancelElasticsearchServiceSoftwareUpdateResponse pResponseStatus_ =
  CancelElasticsearchServiceSoftwareUpdateResponse'
    { serviceSoftwareOptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the Elasticsearch service software update.
--
-- /Note:/ Consider using 'serviceSoftwareOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cessursServiceSoftwareOptions :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdateResponse (Lude.Maybe ServiceSoftwareOptions)
cessursServiceSoftwareOptions = Lens.lens (serviceSoftwareOptions :: CancelElasticsearchServiceSoftwareUpdateResponse -> Lude.Maybe ServiceSoftwareOptions) (\s a -> s {serviceSoftwareOptions = a} :: CancelElasticsearchServiceSoftwareUpdateResponse)
{-# DEPRECATED cessursServiceSoftwareOptions "Use generic-lens or generic-optics with 'serviceSoftwareOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cessursResponseStatus :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdateResponse Lude.Int
cessursResponseStatus = Lens.lens (responseStatus :: CancelElasticsearchServiceSoftwareUpdateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelElasticsearchServiceSoftwareUpdateResponse)
{-# DEPRECATED cessursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
