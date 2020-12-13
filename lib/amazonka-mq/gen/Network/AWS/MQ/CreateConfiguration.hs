{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.CreateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version).
module Network.AWS.MQ.CreateConfiguration
  ( -- * Creating a request
    CreateConfiguration (..),
    mkCreateConfiguration,

    -- ** Request lenses
    ccEngineVersion,
    ccAuthenticationStrategy,
    ccName,
    ccEngineType,
    ccTags,

    -- * Destructuring the response
    CreateConfigurationResponse (..),
    mkCreateConfigurationResponse,

    -- ** Response lenses
    ccrsARN,
    ccrsLatestRevision,
    ccrsCreated,
    ccrsAuthenticationStrategy,
    ccrsName,
    ccrsId,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version).
--
-- /See:/ 'mkCreateConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { -- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Lude.Maybe Lude.Text,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    -- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
    name :: Lude.Maybe Lude.Text,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
    engineType :: Lude.Maybe EngineType,
    -- | Create tags when creating the configuration.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfiguration' with the minimum fields required to make a request.
--
-- * 'engineVersion' - Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'authenticationStrategy' - The authentication strategy associated with the configuration.
-- * 'name' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
-- * 'engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
-- * 'tags' - Create tags when creating the configuration.
mkCreateConfiguration ::
  CreateConfiguration
mkCreateConfiguration =
  CreateConfiguration'
    { engineVersion = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      name = Lude.Nothing,
      engineType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngineVersion :: Lens.Lens' CreateConfiguration (Lude.Maybe Lude.Text)
ccEngineVersion = Lens.lens (engineVersion :: CreateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateConfiguration)
{-# DEPRECATED ccEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthenticationStrategy :: Lens.Lens' CreateConfiguration (Lude.Maybe AuthenticationStrategy)
ccAuthenticationStrategy = Lens.lens (authenticationStrategy :: CreateConfiguration -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: CreateConfiguration)
{-# DEPRECATED ccAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' CreateConfiguration (Lude.Maybe Lude.Text)
ccName = Lens.lens (name :: CreateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateConfiguration)
{-# DEPRECATED ccName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngineType :: Lens.Lens' CreateConfiguration (Lude.Maybe EngineType)
ccEngineType = Lens.lens (engineType :: CreateConfiguration -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: CreateConfiguration)
{-# DEPRECATED ccEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | Create tags when creating the configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccTags = Lens.lens (tags :: CreateConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateConfiguration)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateConfiguration where
  type Rs CreateConfiguration = CreateConfigurationResponse
  request = Req.postJSON mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateConfigurationResponse'
            Lude.<$> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "latestRevision")
            Lude.<*> (x Lude..?> "created")
            Lude.<*> (x Lude..?> "authenticationStrategy")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConfiguration where
  toJSON CreateConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("engineVersion" Lude..=) Lude.<$> engineVersion,
            ("authenticationStrategy" Lude..=) Lude.<$> authenticationStrategy,
            ("name" Lude..=) Lude.<$> name,
            ("engineType" Lude..=) Lude.<$> engineType,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateConfiguration where
  toPath = Lude.const "/v1/configurations"

instance Lude.ToQuery CreateConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { -- | Required. The Amazon Resource Name (ARN) of the configuration.
    arn :: Lude.Maybe Lude.Text,
    -- | The latest revision of the configuration.
    latestRevision :: Lude.Maybe ConfigurationRevision,
    -- | Required. The date and time of the configuration.
    created :: Lude.Maybe Lude.Timestamp,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    -- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
    name :: Lude.Maybe Lude.Text,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'arn' - Required. The Amazon Resource Name (ARN) of the configuration.
-- * 'latestRevision' - The latest revision of the configuration.
-- * 'created' - Required. The date and time of the configuration.
-- * 'authenticationStrategy' - The authentication strategy associated with the configuration.
-- * 'name' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
-- * 'id' - Required. The unique ID that Amazon MQ generates for the configuration.
-- * 'responseStatus' - The response status code.
mkCreateConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConfigurationResponse
mkCreateConfigurationResponse pResponseStatus_ =
  CreateConfigurationResponse'
    { arn = Lude.Nothing,
      latestRevision = Lude.Nothing,
      created = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Required. The Amazon Resource Name (ARN) of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsARN :: Lens.Lens' CreateConfigurationResponse (Lude.Maybe Lude.Text)
ccrsARN = Lens.lens (arn :: CreateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsLatestRevision :: Lens.Lens' CreateConfigurationResponse (Lude.Maybe ConfigurationRevision)
ccrsLatestRevision = Lens.lens (latestRevision :: CreateConfigurationResponse -> Lude.Maybe ConfigurationRevision) (\s a -> s {latestRevision = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsLatestRevision "Use generic-lens or generic-optics with 'latestRevision' instead." #-}

-- | Required. The date and time of the configuration.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCreated :: Lens.Lens' CreateConfigurationResponse (Lude.Maybe Lude.Timestamp)
ccrsCreated = Lens.lens (created :: CreateConfigurationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsAuthenticationStrategy :: Lens.Lens' CreateConfigurationResponse (Lude.Maybe AuthenticationStrategy)
ccrsAuthenticationStrategy = Lens.lens (authenticationStrategy :: CreateConfigurationResponse -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsName :: Lens.Lens' CreateConfigurationResponse (Lude.Maybe Lude.Text)
ccrsName = Lens.lens (name :: CreateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsId :: Lens.Lens' CreateConfigurationResponse (Lude.Maybe Lude.Text)
ccrsId = Lens.lens (id :: CreateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateConfigurationResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConfigurationResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
