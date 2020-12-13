{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified configuration.
module Network.AWS.MQ.DescribeConfiguration
  ( -- * Creating a request
    DescribeConfiguration (..),
    mkDescribeConfiguration,

    -- ** Request lenses
    dcConfigurationId,

    -- * Destructuring the response
    DescribeConfigurationResponse (..),
    mkDescribeConfigurationResponse,

    -- ** Response lenses
    dcrsEngineVersion,
    dcrsARN,
    dcrsLatestRevision,
    dcrsCreated,
    dcrsAuthenticationStrategy,
    dcrsName,
    dcrsId,
    dcrsDescription,
    dcrsEngineType,
    dcrsTags,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConfiguration' smart constructor.
newtype DescribeConfiguration = DescribeConfiguration'
  { -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfiguration' with the minimum fields required to make a request.
--
-- * 'configurationId' - The unique ID that Amazon MQ generates for the configuration.
mkDescribeConfiguration ::
  -- | 'configurationId'
  Lude.Text ->
  DescribeConfiguration
mkDescribeConfiguration pConfigurationId_ =
  DescribeConfiguration' {configurationId = pConfigurationId_}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConfigurationId :: Lens.Lens' DescribeConfiguration Lude.Text
dcConfigurationId = Lens.lens (configurationId :: DescribeConfiguration -> Lude.Text) (\s a -> s {configurationId = a} :: DescribeConfiguration)
{-# DEPRECATED dcConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.AWSRequest DescribeConfiguration where
  type Rs DescribeConfiguration = DescribeConfigurationResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationResponse'
            Lude.<$> (x Lude..?> "engineVersion")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "latestRevision")
            Lude.<*> (x Lude..?> "created")
            Lude.<*> (x Lude..?> "authenticationStrategy")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "engineType")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeConfiguration where
  toPath DescribeConfiguration' {..} =
    Lude.mconcat ["/v1/configurations/", Lude.toBS configurationId]

instance Lude.ToQuery DescribeConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConfigurationResponse' smart constructor.
data DescribeConfigurationResponse = DescribeConfigurationResponse'
  { -- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Lude.Maybe Lude.Text,
    -- | Required. The ARN of the configuration.
    arn :: Lude.Maybe Lude.Text,
    -- | Required. The latest revision of the configuration.
    latestRevision :: Lude.Maybe ConfigurationRevision,
    -- | Required. The date and time of the configuration revision.
    created :: Lude.Maybe Lude.Timestamp,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    -- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
    name :: Lude.Maybe Lude.Text,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Lude.Maybe Lude.Text,
    -- | Required. The description of the configuration.
    description :: Lude.Maybe Lude.Text,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
    engineType :: Lude.Maybe EngineType,
    -- | The list of all tags associated with this configuration.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'engineVersion' - Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'arn' - Required. The ARN of the configuration.
-- * 'latestRevision' - Required. The latest revision of the configuration.
-- * 'created' - Required. The date and time of the configuration revision.
-- * 'authenticationStrategy' - The authentication strategy associated with the configuration.
-- * 'name' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
-- * 'id' - Required. The unique ID that Amazon MQ generates for the configuration.
-- * 'description' - Required. The description of the configuration.
-- * 'engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
-- * 'tags' - The list of all tags associated with this configuration.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationResponse
mkDescribeConfigurationResponse pResponseStatus_ =
  DescribeConfigurationResponse'
    { engineVersion = Lude.Nothing,
      arn = Lude.Nothing,
      latestRevision = Lude.Nothing,
      created = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing,
      engineType = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsEngineVersion :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe Lude.Text)
dcrsEngineVersion = Lens.lens (engineVersion :: DescribeConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Required. The ARN of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsARN :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe Lude.Text)
dcrsARN = Lens.lens (arn :: DescribeConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Required. The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsLatestRevision :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe ConfigurationRevision)
dcrsLatestRevision = Lens.lens (latestRevision :: DescribeConfigurationResponse -> Lude.Maybe ConfigurationRevision) (\s a -> s {latestRevision = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsLatestRevision "Use generic-lens or generic-optics with 'latestRevision' instead." #-}

-- | Required. The date and time of the configuration revision.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCreated :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe Lude.Timestamp)
dcrsCreated = Lens.lens (created :: DescribeConfigurationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsAuthenticationStrategy :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe AuthenticationStrategy)
dcrsAuthenticationStrategy = Lens.lens (authenticationStrategy :: DescribeConfigurationResponse -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsName :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe Lude.Text)
dcrsName = Lens.lens (name :: DescribeConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsId :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe Lude.Text)
dcrsId = Lens.lens (id :: DescribeConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Required. The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsDescription :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe Lude.Text)
dcrsDescription = Lens.lens (description :: DescribeConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsEngineType :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe EngineType)
dcrsEngineType = Lens.lens (engineType :: DescribeConfigurationResponse -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The list of all tags associated with this configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsTags :: Lens.Lens' DescribeConfigurationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dcrsTags = Lens.lens (tags :: DescribeConfigurationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeConfigurationResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
