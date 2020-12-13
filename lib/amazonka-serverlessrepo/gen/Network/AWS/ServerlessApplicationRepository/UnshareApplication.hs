{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.UnshareApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unshares an application from an AWS Organization.
--
-- This operation can be called only from the organization's master account.
module Network.AWS.ServerlessApplicationRepository.UnshareApplication
  ( -- * Creating a request
    UnshareApplication (..),
    mkUnshareApplication,

    -- ** Request lenses
    uApplicationId,
    uOrganizationId,

    -- * Destructuring the response
    UnshareApplicationResponse (..),
    mkUnshareApplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkUnshareApplication' smart constructor.
data UnshareApplication = UnshareApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text,
    -- | The AWS Organization ID to unshare the application from.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnshareApplication' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'organizationId' - The AWS Organization ID to unshare the application from.
mkUnshareApplication ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  UnshareApplication
mkUnshareApplication pApplicationId_ pOrganizationId_ =
  UnshareApplication'
    { applicationId = pApplicationId_,
      organizationId = pOrganizationId_
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uApplicationId :: Lens.Lens' UnshareApplication Lude.Text
uApplicationId = Lens.lens (applicationId :: UnshareApplication -> Lude.Text) (\s a -> s {applicationId = a} :: UnshareApplication)
{-# DEPRECATED uApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The AWS Organization ID to unshare the application from.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uOrganizationId :: Lens.Lens' UnshareApplication Lude.Text
uOrganizationId = Lens.lens (organizationId :: UnshareApplication -> Lude.Text) (\s a -> s {organizationId = a} :: UnshareApplication)
{-# DEPRECATED uOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest UnshareApplication where
  type Rs UnshareApplication = UnshareApplicationResponse
  request = Req.postJSON serverlessApplicationRepositoryService
  response = Res.receiveNull UnshareApplicationResponse'

instance Lude.ToHeaders UnshareApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnshareApplication where
  toJSON UnshareApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("organizationId" Lude..= organizationId)]
      )

instance Lude.ToPath UnshareApplication where
  toPath UnshareApplication' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/unshare"]

instance Lude.ToQuery UnshareApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnshareApplicationResponse' smart constructor.
data UnshareApplicationResponse = UnshareApplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnshareApplicationResponse' with the minimum fields required to make a request.
mkUnshareApplicationResponse ::
  UnshareApplicationResponse
mkUnshareApplicationResponse = UnshareApplicationResponse'
