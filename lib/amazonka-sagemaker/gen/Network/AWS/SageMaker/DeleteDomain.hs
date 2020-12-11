{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a domain. If you onboarded with IAM mode, you will need to delete your domain to onboard again using SSO. Use with caution. All of the members of the domain will lose access to their EFS volume, including data, notebooks, and other artifacts.
module Network.AWS.SageMaker.DeleteDomain
  ( -- * Creating a request
    DeleteDomain (..),
    mkDeleteDomain,

    -- ** Request lenses
    dddRetentionPolicy,
    dddDomainId,

    -- * Destructuring the response
    DeleteDomainResponse (..),
    mkDeleteDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { retentionPolicy ::
      Lude.Maybe RetentionPolicy,
    domainId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- * 'domainId' - The domain ID.
-- * 'retentionPolicy' - The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted).
mkDeleteDomain ::
  -- | 'domainId'
  Lude.Text ->
  DeleteDomain
mkDeleteDomain pDomainId_ =
  DeleteDomain'
    { retentionPolicy = Lude.Nothing,
      domainId = pDomainId_
    }

-- | The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted).
--
-- /Note:/ Consider using 'retentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddRetentionPolicy :: Lens.Lens' DeleteDomain (Lude.Maybe RetentionPolicy)
dddRetentionPolicy = Lens.lens (retentionPolicy :: DeleteDomain -> Lude.Maybe RetentionPolicy) (\s a -> s {retentionPolicy = a} :: DeleteDomain)
{-# DEPRECATED dddRetentionPolicy "Use generic-lens or generic-optics with 'retentionPolicy' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddDomainId :: Lens.Lens' DeleteDomain Lude.Text
dddDomainId = Lens.lens (domainId :: DeleteDomain -> Lude.Text) (\s a -> s {domainId = a} :: DeleteDomain)
{-# DEPRECATED dddDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteDomainResponse'

instance Lude.ToHeaders DeleteDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RetentionPolicy" Lude..=) Lude.<$> retentionPolicy,
            Lude.Just ("DomainId" Lude..= domainId)
          ]
      )

instance Lude.ToPath DeleteDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
mkDeleteDomainResponse ::
  DeleteDomainResponse
mkDeleteDomainResponse = DeleteDomainResponse'
