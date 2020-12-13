{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteGitHubAccountToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a GitHub account connection.
module Network.AWS.CodeDeploy.DeleteGitHubAccountToken
  ( -- * Creating a request
    DeleteGitHubAccountToken (..),
    mkDeleteGitHubAccountToken,

    -- ** Request lenses
    dghatTokenName,

    -- * Destructuring the response
    DeleteGitHubAccountTokenResponse (..),
    mkDeleteGitHubAccountTokenResponse,

    -- ** Response lenses
    dghatrsTokenName,
    dghatrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteGitHubAccount@ operation.
--
-- /See:/ 'mkDeleteGitHubAccountToken' smart constructor.
newtype DeleteGitHubAccountToken = DeleteGitHubAccountToken'
  { -- | The name of the GitHub account connection to delete.
    tokenName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGitHubAccountToken' with the minimum fields required to make a request.
--
-- * 'tokenName' - The name of the GitHub account connection to delete.
mkDeleteGitHubAccountToken ::
  DeleteGitHubAccountToken
mkDeleteGitHubAccountToken =
  DeleteGitHubAccountToken' {tokenName = Lude.Nothing}

-- | The name of the GitHub account connection to delete.
--
-- /Note:/ Consider using 'tokenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dghatTokenName :: Lens.Lens' DeleteGitHubAccountToken (Lude.Maybe Lude.Text)
dghatTokenName = Lens.lens (tokenName :: DeleteGitHubAccountToken -> Lude.Maybe Lude.Text) (\s a -> s {tokenName = a} :: DeleteGitHubAccountToken)
{-# DEPRECATED dghatTokenName "Use generic-lens or generic-optics with 'tokenName' instead." #-}

instance Lude.AWSRequest DeleteGitHubAccountToken where
  type Rs DeleteGitHubAccountToken = DeleteGitHubAccountTokenResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteGitHubAccountTokenResponse'
            Lude.<$> (x Lude..?> "tokenName") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGitHubAccountToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.DeleteGitHubAccountToken" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGitHubAccountToken where
  toJSON DeleteGitHubAccountToken' {..} =
    Lude.object
      (Lude.catMaybes [("tokenName" Lude..=) Lude.<$> tokenName])

instance Lude.ToPath DeleteGitHubAccountToken where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGitHubAccountToken where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteGitHubAccountToken@ operation.
--
-- /See:/ 'mkDeleteGitHubAccountTokenResponse' smart constructor.
data DeleteGitHubAccountTokenResponse = DeleteGitHubAccountTokenResponse'
  { -- | The name of the GitHub account connection that was deleted.
    tokenName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGitHubAccountTokenResponse' with the minimum fields required to make a request.
--
-- * 'tokenName' - The name of the GitHub account connection that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteGitHubAccountTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGitHubAccountTokenResponse
mkDeleteGitHubAccountTokenResponse pResponseStatus_ =
  DeleteGitHubAccountTokenResponse'
    { tokenName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the GitHub account connection that was deleted.
--
-- /Note:/ Consider using 'tokenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dghatrsTokenName :: Lens.Lens' DeleteGitHubAccountTokenResponse (Lude.Maybe Lude.Text)
dghatrsTokenName = Lens.lens (tokenName :: DeleteGitHubAccountTokenResponse -> Lude.Maybe Lude.Text) (\s a -> s {tokenName = a} :: DeleteGitHubAccountTokenResponse)
{-# DEPRECATED dghatrsTokenName "Use generic-lens or generic-optics with 'tokenName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dghatrsResponseStatus :: Lens.Lens' DeleteGitHubAccountTokenResponse Lude.Int
dghatrsResponseStatus = Lens.lens (responseStatus :: DeleteGitHubAccountTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGitHubAccountTokenResponse)
{-# DEPRECATED dghatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
