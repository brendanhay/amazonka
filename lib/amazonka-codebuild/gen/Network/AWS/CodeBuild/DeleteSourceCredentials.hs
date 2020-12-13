{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source credentials.
module Network.AWS.CodeBuild.DeleteSourceCredentials
  ( -- * Creating a request
    DeleteSourceCredentials (..),
    mkDeleteSourceCredentials,

    -- ** Request lenses
    dscArn,

    -- * Destructuring the response
    DeleteSourceCredentialsResponse (..),
    mkDeleteSourceCredentialsResponse,

    -- ** Response lenses
    dscrsArn,
    dscrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSourceCredentials' smart constructor.
newtype DeleteSourceCredentials = DeleteSourceCredentials'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSourceCredentials' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the token.
mkDeleteSourceCredentials ::
  -- | 'arn'
  Lude.Text ->
  DeleteSourceCredentials
mkDeleteSourceCredentials pArn_ =
  DeleteSourceCredentials' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the token.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscArn :: Lens.Lens' DeleteSourceCredentials Lude.Text
dscArn = Lens.lens (arn :: DeleteSourceCredentials -> Lude.Text) (\s a -> s {arn = a} :: DeleteSourceCredentials)
{-# DEPRECATED dscArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteSourceCredentials where
  type Rs DeleteSourceCredentials = DeleteSourceCredentialsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSourceCredentialsResponse'
            Lude.<$> (x Lude..?> "arn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSourceCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DeleteSourceCredentials" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSourceCredentials where
  toJSON DeleteSourceCredentials' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteSourceCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSourceCredentials where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSourceCredentialsResponse' smart constructor.
data DeleteSourceCredentialsResponse = DeleteSourceCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSourceCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the token.
-- * 'responseStatus' - The response status code.
mkDeleteSourceCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSourceCredentialsResponse
mkDeleteSourceCredentialsResponse pResponseStatus_ =
  DeleteSourceCredentialsResponse'
    { arn = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsArn :: Lens.Lens' DeleteSourceCredentialsResponse (Lude.Maybe Lude.Text)
dscrsArn = Lens.lens (arn :: DeleteSourceCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteSourceCredentialsResponse)
{-# DEPRECATED dscrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DeleteSourceCredentialsResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DeleteSourceCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSourceCredentialsResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
