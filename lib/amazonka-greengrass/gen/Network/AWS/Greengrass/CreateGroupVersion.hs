{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateGroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a group which has already been defined.
module Network.AWS.Greengrass.CreateGroupVersion
  ( -- * Creating a request
    CreateGroupVersion (..),
    mkCreateGroupVersion,

    -- ** Request lenses
    cgvAmznClientToken,
    cgvResourceDefinitionVersionARN,
    cgvSubscriptionDefinitionVersionARN,
    cgvCoreDefinitionVersionARN,
    cgvGroupId,
    cgvDeviceDefinitionVersionARN,
    cgvFunctionDefinitionVersionARN,
    cgvLoggerDefinitionVersionARN,
    cgvConnectorDefinitionVersionARN,

    -- * Destructuring the response
    CreateGroupVersionResponse (..),
    mkCreateGroupVersionResponse,

    -- ** Response lenses
    cgvrsARN,
    cgvrsCreationTimestamp,
    cgvrsVersion,
    cgvrsId,
    cgvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGroupVersion' smart constructor.
data CreateGroupVersion = CreateGroupVersion'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the core definition version for this group.
    coreDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Lude.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroupVersion' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'resourceDefinitionVersionARN' - The ARN of the resource definition version for this group.
-- * 'subscriptionDefinitionVersionARN' - The ARN of the subscription definition version for this group.
-- * 'coreDefinitionVersionARN' - The ARN of the core definition version for this group.
-- * 'groupId' - The ID of the Greengrass group.
-- * 'deviceDefinitionVersionARN' - The ARN of the device definition version for this group.
-- * 'functionDefinitionVersionARN' - The ARN of the function definition version for this group.
-- * 'loggerDefinitionVersionARN' - The ARN of the logger definition version for this group.
-- * 'connectorDefinitionVersionARN' - The ARN of the connector definition version for this group.
mkCreateGroupVersion ::
  -- | 'groupId'
  Lude.Text ->
  CreateGroupVersion
mkCreateGroupVersion pGroupId_ =
  CreateGroupVersion'
    { amznClientToken = Lude.Nothing,
      resourceDefinitionVersionARN = Lude.Nothing,
      subscriptionDefinitionVersionARN = Lude.Nothing,
      coreDefinitionVersionARN = Lude.Nothing,
      groupId = pGroupId_,
      deviceDefinitionVersionARN = Lude.Nothing,
      functionDefinitionVersionARN = Lude.Nothing,
      loggerDefinitionVersionARN = Lude.Nothing,
      connectorDefinitionVersionARN = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvAmznClientToken :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvAmznClientToken = Lens.lens (amznClientToken :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateGroupVersion)
{-# DEPRECATED cgvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The ARN of the resource definition version for this group.
--
-- /Note:/ Consider using 'resourceDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvResourceDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvResourceDefinitionVersionARN = Lens.lens (resourceDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {resourceDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvResourceDefinitionVersionARN "Use generic-lens or generic-optics with 'resourceDefinitionVersionARN' instead." #-}

-- | The ARN of the subscription definition version for this group.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvSubscriptionDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvSubscriptionDefinitionVersionARN = Lens.lens (subscriptionDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvSubscriptionDefinitionVersionARN "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionARN' instead." #-}

-- | The ARN of the core definition version for this group.
--
-- /Note:/ Consider using 'coreDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvCoreDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvCoreDefinitionVersionARN = Lens.lens (coreDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {coreDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvCoreDefinitionVersionARN "Use generic-lens or generic-optics with 'coreDefinitionVersionARN' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvGroupId :: Lens.Lens' CreateGroupVersion Lude.Text
cgvGroupId = Lens.lens (groupId :: CreateGroupVersion -> Lude.Text) (\s a -> s {groupId = a} :: CreateGroupVersion)
{-# DEPRECATED cgvGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The ARN of the device definition version for this group.
--
-- /Note:/ Consider using 'deviceDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvDeviceDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvDeviceDefinitionVersionARN = Lens.lens (deviceDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {deviceDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvDeviceDefinitionVersionARN "Use generic-lens or generic-optics with 'deviceDefinitionVersionARN' instead." #-}

-- | The ARN of the function definition version for this group.
--
-- /Note:/ Consider using 'functionDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvFunctionDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvFunctionDefinitionVersionARN = Lens.lens (functionDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {functionDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvFunctionDefinitionVersionARN "Use generic-lens or generic-optics with 'functionDefinitionVersionARN' instead." #-}

-- | The ARN of the logger definition version for this group.
--
-- /Note:/ Consider using 'loggerDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvLoggerDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvLoggerDefinitionVersionARN = Lens.lens (loggerDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {loggerDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvLoggerDefinitionVersionARN "Use generic-lens or generic-optics with 'loggerDefinitionVersionARN' instead." #-}

-- | The ARN of the connector definition version for this group.
--
-- /Note:/ Consider using 'connectorDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvConnectorDefinitionVersionARN :: Lens.Lens' CreateGroupVersion (Lude.Maybe Lude.Text)
cgvConnectorDefinitionVersionARN = Lens.lens (connectorDefinitionVersionARN :: CreateGroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {connectorDefinitionVersionARN = a} :: CreateGroupVersion)
{-# DEPRECATED cgvConnectorDefinitionVersionARN "Use generic-lens or generic-optics with 'connectorDefinitionVersionARN' instead." #-}

instance Lude.AWSRequest CreateGroupVersion where
  type Rs CreateGroupVersion = CreateGroupVersionResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupVersionResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroupVersion where
  toHeaders CreateGroupVersion' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateGroupVersion where
  toJSON CreateGroupVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceDefinitionVersionArn" Lude..=)
              Lude.<$> resourceDefinitionVersionARN,
            ("SubscriptionDefinitionVersionArn" Lude..=)
              Lude.<$> subscriptionDefinitionVersionARN,
            ("CoreDefinitionVersionArn" Lude..=)
              Lude.<$> coreDefinitionVersionARN,
            ("DeviceDefinitionVersionArn" Lude..=)
              Lude.<$> deviceDefinitionVersionARN,
            ("FunctionDefinitionVersionArn" Lude..=)
              Lude.<$> functionDefinitionVersionARN,
            ("LoggerDefinitionVersionArn" Lude..=)
              Lude.<$> loggerDefinitionVersionARN,
            ("ConnectorDefinitionVersionArn" Lude..=)
              Lude.<$> connectorDefinitionVersionARN
          ]
      )

instance Lude.ToPath CreateGroupVersion where
  toPath CreateGroupVersion' {..} =
    Lude.mconcat
      ["/greengrass/groups/", Lude.toBS groupId, "/versions"]

instance Lude.ToQuery CreateGroupVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupVersionResponse' smart constructor.
data CreateGroupVersionResponse = CreateGroupVersionResponse'
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

-- | Creates a value of 'CreateGroupVersionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'version' - The ID of the version.
-- * 'id' - The ID of the parent definition that the version is associated with.
-- * 'responseStatus' - The response status code.
mkCreateGroupVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupVersionResponse
mkCreateGroupVersionResponse pResponseStatus_ =
  CreateGroupVersionResponse'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrsARN :: Lens.Lens' CreateGroupVersionResponse (Lude.Maybe Lude.Text)
cgvrsARN = Lens.lens (arn :: CreateGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateGroupVersionResponse)
{-# DEPRECATED cgvrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrsCreationTimestamp :: Lens.Lens' CreateGroupVersionResponse (Lude.Maybe Lude.Text)
cgvrsCreationTimestamp = Lens.lens (creationTimestamp :: CreateGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: CreateGroupVersionResponse)
{-# DEPRECATED cgvrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrsVersion :: Lens.Lens' CreateGroupVersionResponse (Lude.Maybe Lude.Text)
cgvrsVersion = Lens.lens (version :: CreateGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateGroupVersionResponse)
{-# DEPRECATED cgvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrsId :: Lens.Lens' CreateGroupVersionResponse (Lude.Maybe Lude.Text)
cgvrsId = Lens.lens (id :: CreateGroupVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateGroupVersionResponse)
{-# DEPRECATED cgvrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrsResponseStatus :: Lens.Lens' CreateGroupVersionResponse Lude.Int
cgvrsResponseStatus = Lens.lens (responseStatus :: CreateGroupVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupVersionResponse)
{-# DEPRECATED cgvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
