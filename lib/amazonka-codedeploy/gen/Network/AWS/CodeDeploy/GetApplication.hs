{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application.
module Network.AWS.CodeDeploy.GetApplication
  ( -- * Creating a request
    GetApplication (..),
    mkGetApplication,

    -- ** Request lenses
    gaApplicationName,

    -- * Destructuring the response
    GetApplicationResponse (..),
    mkGetApplicationResponse,

    -- ** Response lenses
    garsApplication,
    garsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetApplication@ operation.
--
-- /See:/ 'mkGetApplication' smart constructor.
newtype GetApplication = GetApplication'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
mkGetApplication ::
  -- | 'applicationName'
  Lude.Text ->
  GetApplication
mkGetApplication pApplicationName_ =
  GetApplication' {applicationName = pApplicationName_}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApplicationName :: Lens.Lens' GetApplication Lude.Text
gaApplicationName = Lens.lens (applicationName :: GetApplication -> Lude.Text) (\s a -> s {applicationName = a} :: GetApplication)
{-# DEPRECATED gaApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest GetApplication where
  type Rs GetApplication = GetApplicationResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Lude.<$> (x Lude..?> "application") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetApplication where
  toJSON GetApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("applicationName" Lude..= applicationName)]
      )

instance Lude.ToPath GetApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery GetApplication where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetApplication@ operation.
--
-- /See:/ 'mkGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | Information about the application.
    application :: Lude.Maybe ApplicationInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationResponse' with the minimum fields required to make a request.
--
-- * 'application' - Information about the application.
-- * 'responseStatus' - The response status code.
mkGetApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetApplicationResponse
mkGetApplicationResponse pResponseStatus_ =
  GetApplicationResponse'
    { application = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the application.
--
-- /Note:/ Consider using 'application' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsApplication :: Lens.Lens' GetApplicationResponse (Lude.Maybe ApplicationInfo)
garsApplication = Lens.lens (application :: GetApplicationResponse -> Lude.Maybe ApplicationInfo) (\s a -> s {application = a} :: GetApplicationResponse)
{-# DEPRECATED garsApplication "Use generic-lens or generic-optics with 'application' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetApplicationResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApplicationResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
