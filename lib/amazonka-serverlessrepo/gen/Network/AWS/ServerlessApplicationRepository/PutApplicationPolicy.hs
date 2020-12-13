{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permission policy for an application. For the list of actions supported for this operation, see
--
--  <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
--  Permissions>
--  .
module Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
  ( -- * Creating a request
    PutApplicationPolicy (..),
    mkPutApplicationPolicy,

    -- ** Request lenses
    papApplicationId,
    papStatements,

    -- * Destructuring the response
    PutApplicationPolicyResponse (..),
    mkPutApplicationPolicyResponse,

    -- ** Response lenses
    paprsStatements,
    paprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkPutApplicationPolicy' smart constructor.
data PutApplicationPolicy = PutApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text,
    -- | An array of policy statements applied to the application.
    statements :: [ApplicationPolicyStatement]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutApplicationPolicy' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'statements' - An array of policy statements applied to the application.
mkPutApplicationPolicy ::
  -- | 'applicationId'
  Lude.Text ->
  PutApplicationPolicy
mkPutApplicationPolicy pApplicationId_ =
  PutApplicationPolicy'
    { applicationId = pApplicationId_,
      statements = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papApplicationId :: Lens.Lens' PutApplicationPolicy Lude.Text
papApplicationId = Lens.lens (applicationId :: PutApplicationPolicy -> Lude.Text) (\s a -> s {applicationId = a} :: PutApplicationPolicy)
{-# DEPRECATED papApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStatements :: Lens.Lens' PutApplicationPolicy [ApplicationPolicyStatement]
papStatements = Lens.lens (statements :: PutApplicationPolicy -> [ApplicationPolicyStatement]) (\s a -> s {statements = a} :: PutApplicationPolicy)
{-# DEPRECATED papStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

instance Lude.AWSRequest PutApplicationPolicy where
  type Rs PutApplicationPolicy = PutApplicationPolicyResponse
  request = Req.putJSON serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutApplicationPolicyResponse'
            Lude.<$> (x Lude..?> "statements" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutApplicationPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutApplicationPolicy where
  toJSON PutApplicationPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("statements" Lude..= statements)])

instance Lude.ToPath PutApplicationPolicy where
  toPath PutApplicationPolicy' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/policy"]

instance Lude.ToQuery PutApplicationPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutApplicationPolicyResponse' smart constructor.
data PutApplicationPolicyResponse = PutApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Lude.Maybe [ApplicationPolicyStatement],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutApplicationPolicyResponse' with the minimum fields required to make a request.
--
-- * 'statements' - An array of policy statements applied to the application.
-- * 'responseStatus' - The response status code.
mkPutApplicationPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutApplicationPolicyResponse
mkPutApplicationPolicyResponse pResponseStatus_ =
  PutApplicationPolicyResponse'
    { statements = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paprsStatements :: Lens.Lens' PutApplicationPolicyResponse (Lude.Maybe [ApplicationPolicyStatement])
paprsStatements = Lens.lens (statements :: PutApplicationPolicyResponse -> Lude.Maybe [ApplicationPolicyStatement]) (\s a -> s {statements = a} :: PutApplicationPolicyResponse)
{-# DEPRECATED paprsStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paprsResponseStatus :: Lens.Lens' PutApplicationPolicyResponse Lude.Int
paprsResponseStatus = Lens.lens (responseStatus :: PutApplicationPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutApplicationPolicyResponse)
{-# DEPRECATED paprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
