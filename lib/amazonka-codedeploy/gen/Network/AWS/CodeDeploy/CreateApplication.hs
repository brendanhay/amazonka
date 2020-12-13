{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Network.AWS.CodeDeploy.CreateApplication
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caComputePlatform,
    caApplicationName,
    caTags,

    -- * Destructuring the response
    CreateApplicationResponse (..),
    mkCreateApplicationResponse,

    -- ** Response lenses
    carsApplicationId,
    carsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateApplication@ operation.
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
    computePlatform :: Lude.Maybe ComputePlatform,
    -- | The name of the application. This name must be unique with the applicable IAM user or AWS account.
    applicationName :: Lude.Text,
    -- | The metadata that you apply to CodeDeploy applications to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- * 'computePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
-- * 'applicationName' - The name of the application. This name must be unique with the applicable IAM user or AWS account.
-- * 'tags' - The metadata that you apply to CodeDeploy applications to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
mkCreateApplication ::
  -- | 'applicationName'
  Lude.Text ->
  CreateApplication
mkCreateApplication pApplicationName_ =
  CreateApplication'
    { computePlatform = Lude.Nothing,
      applicationName = pApplicationName_,
      tags = Lude.Nothing
    }

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caComputePlatform :: Lens.Lens' CreateApplication (Lude.Maybe ComputePlatform)
caComputePlatform = Lens.lens (computePlatform :: CreateApplication -> Lude.Maybe ComputePlatform) (\s a -> s {computePlatform = a} :: CreateApplication)
{-# DEPRECATED caComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | The name of the application. This name must be unique with the applicable IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Lude.Text
caApplicationName = Lens.lens (applicationName :: CreateApplication -> Lude.Text) (\s a -> s {applicationName = a} :: CreateApplication)
{-# DEPRECATED caApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The metadata that you apply to CodeDeploy applications to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateApplication -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateApplication)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateApplication where
  type Rs CreateApplication = CreateApplicationResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Lude.<$> (x Lude..?> "applicationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.CreateApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("computePlatform" Lude..=) Lude.<$> computePlatform,
            Lude.Just ("applicationName" Lude..= applicationName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApplication where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateApplication@ operation.
--
-- /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | A unique application ID.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - A unique application ID.
-- * 'responseStatus' - The response status code.
mkCreateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateApplicationResponse
mkCreateApplicationResponse pResponseStatus_ =
  CreateApplicationResponse'
    { applicationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique application ID.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsApplicationId :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsApplicationId = Lens.lens (applicationId :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: CreateApplicationResponse)
{-# DEPRECATED carsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateApplicationResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateApplicationResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
