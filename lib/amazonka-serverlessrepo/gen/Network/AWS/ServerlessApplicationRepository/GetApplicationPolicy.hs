{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy for the application.
module Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
  ( -- * Creating a request
    GetApplicationPolicy (..),
    mkGetApplicationPolicy,

    -- ** Request lenses
    gapApplicationId,

    -- * Destructuring the response
    GetApplicationPolicyResponse (..),
    mkGetApplicationPolicyResponse,

    -- ** Response lenses
    gaprsStatements,
    gaprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkGetApplicationPolicy' smart constructor.
newtype GetApplicationPolicy = GetApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationPolicy' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
mkGetApplicationPolicy ::
  -- | 'applicationId'
  Lude.Text ->
  GetApplicationPolicy
mkGetApplicationPolicy pApplicationId_ =
  GetApplicationPolicy' {applicationId = pApplicationId_}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapApplicationId :: Lens.Lens' GetApplicationPolicy Lude.Text
gapApplicationId = Lens.lens (applicationId :: GetApplicationPolicy -> Lude.Text) (\s a -> s {applicationId = a} :: GetApplicationPolicy)
{-# DEPRECATED gapApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetApplicationPolicy where
  type Rs GetApplicationPolicy = GetApplicationPolicyResponse
  request = Req.get serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApplicationPolicyResponse'
            Lude.<$> (x Lude..?> "statements" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApplicationPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetApplicationPolicy where
  toPath GetApplicationPolicy' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/policy"]

instance Lude.ToQuery GetApplicationPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetApplicationPolicyResponse' smart constructor.
data GetApplicationPolicyResponse = GetApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Lude.Maybe [ApplicationPolicyStatement],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationPolicyResponse' with the minimum fields required to make a request.
--
-- * 'statements' - An array of policy statements applied to the application.
-- * 'responseStatus' - The response status code.
mkGetApplicationPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetApplicationPolicyResponse
mkGetApplicationPolicyResponse pResponseStatus_ =
  GetApplicationPolicyResponse'
    { statements = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaprsStatements :: Lens.Lens' GetApplicationPolicyResponse (Lude.Maybe [ApplicationPolicyStatement])
gaprsStatements = Lens.lens (statements :: GetApplicationPolicyResponse -> Lude.Maybe [ApplicationPolicyStatement]) (\s a -> s {statements = a} :: GetApplicationPolicyResponse)
{-# DEPRECATED gaprsStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaprsResponseStatus :: Lens.Lens' GetApplicationPolicyResponse Lude.Int
gaprsResponseStatus = Lens.lens (responseStatus :: GetApplicationPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApplicationPolicyResponse)
{-# DEPRECATED gaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
