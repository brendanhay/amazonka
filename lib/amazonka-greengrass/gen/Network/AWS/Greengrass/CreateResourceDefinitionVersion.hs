{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a resource definition that has already been defined.
module Network.AWS.Greengrass.CreateResourceDefinitionVersion
  ( -- * Creating a request
    CreateResourceDefinitionVersion (..),
    mkCreateResourceDefinitionVersion,

    -- ** Request lenses
    crdvResourceDefinitionId,
    crdvAmznClientToken,
    crdvResources,

    -- * Destructuring the response
    CreateResourceDefinitionVersionResponse (..),
    mkCreateResourceDefinitionVersionResponse,

    -- ** Response lenses
    crdvrsARN,
    crdvrsCreationTimestamp,
    crdvrsVersion,
    crdvrsId,
    crdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateResourceDefinitionVersion' smart constructor.
data CreateResourceDefinitionVersion = CreateResourceDefinitionVersion'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Lude.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | A list of resources.
    resources :: Lude.Maybe [Resource]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'resourceDefinitionId' - The ID of the resource definition.
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'resources' - A list of resources.
mkCreateResourceDefinitionVersion ::
  -- | 'resourceDefinitionId'
  Lude.Text ->
  CreateResourceDefinitionVersion
mkCreateResourceDefinitionVersion pResourceDefinitionId_ =
  CreateResourceDefinitionVersion'
    { resourceDefinitionId =
        pResourceDefinitionId_,
      amznClientToken = Lude.Nothing,
      resources = Lude.Nothing
    }

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvResourceDefinitionId :: Lens.Lens' CreateResourceDefinitionVersion Lude.Text
crdvResourceDefinitionId = Lens.lens (resourceDefinitionId :: CreateResourceDefinitionVersion -> Lude.Text) (\s a -> s {resourceDefinitionId = a} :: CreateResourceDefinitionVersion)
{-# DEPRECATED crdvResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvAmznClientToken :: Lens.Lens' CreateResourceDefinitionVersion (Lude.Maybe Lude.Text)
crdvAmznClientToken = Lens.lens (amznClientToken :: CreateResourceDefinitionVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateResourceDefinitionVersion)
{-# DEPRECATED crdvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of resources.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvResources :: Lens.Lens' CreateResourceDefinitionVersion (Lude.Maybe [Resource])
crdvResources = Lens.lens (resources :: CreateResourceDefinitionVersion -> Lude.Maybe [Resource]) (\s a -> s {resources = a} :: CreateResourceDefinitionVersion)
{-# DEPRECATED crdvResources "Use generic-lens or generic-optics with 'resources' instead." #-}

instance Lude.AWSRequest CreateResourceDefinitionVersion where
  type
    Rs CreateResourceDefinitionVersion =
      CreateResourceDefinitionVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateResourceDefinitionVersion where
  toHeaders CreateResourceDefinitionVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateResourceDefinitionVersion where
  toJSON CreateResourceDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Resources" Lude..=) Lude.<$> resources])

instance Lude.ToPath CreateResourceDefinitionVersion where
  toPath CreateResourceDefinitionVersion' {..} =
    Lude.mconcat
      [ "/greengrass/definition/resources/",
        Lude.toBS resourceDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery CreateResourceDefinitionVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResourceDefinitionVersionResponse' smart constructor.
data CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'version' - The ID of the version.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
mkCreateResourceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateResourceDefinitionVersionResponse
mkCreateResourceDefinitionVersionResponse pResponseStatus_ =
  CreateResourceDefinitionVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrsARN :: Lens.Lens' CreateResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
crdvrsARN = Lens.lens (arn :: CreateResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateResourceDefinitionVersionResponse)
{-# DEPRECATED crdvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrsCreationTimestamp :: Lens.Lens' CreateResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
crdvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateResourceDefinitionVersionResponse)
{-# DEPRECATED crdvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrsVersion :: Lens.Lens' CreateResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
crdvrsVersion = Lens.lens (version :: CreateResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateResourceDefinitionVersionResponse)
{-# DEPRECATED crdvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrsId :: Lens.Lens' CreateResourceDefinitionVersionResponse (Lude.Maybe Lude.Text)
crdvrsId = Lens.lens (id :: CreateResourceDefinitionVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateResourceDefinitionVersionResponse)
{-# DEPRECATED crdvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdvrsResponseStatus :: Lens.Lens' CreateResourceDefinitionVersionResponse Lude.Int
crdvrsResponseStatus = Lens.lens (responseStatus :: CreateResourceDefinitionVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResourceDefinitionVersionResponse)
{-# DEPRECATED crdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
