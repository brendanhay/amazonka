{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Configuration
  ( Configuration (..),

    -- * Smart constructor
    mkConfiguration,

    -- * Lenses
    cEngineVersion,
    cARN,
    cLatestRevision,
    cCreated,
    cAuthenticationStrategy,
    cName,
    cId,
    cDescription,
    cEngineType,
    cTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.AuthenticationStrategy
import Network.AWS.MQ.Types.ConfigurationRevision
import Network.AWS.MQ.Types.EngineType
import qualified Network.AWS.Prelude as Lude

-- | Returns information about all configurations.
--
-- /See:/ 'mkConfiguration' smart constructor.
data Configuration = Configuration'
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
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
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
mkConfiguration ::
  Configuration
mkConfiguration =
  Configuration'
    { engineVersion = Lude.Nothing,
      arn = Lude.Nothing,
      latestRevision = Lude.Nothing,
      created = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing,
      engineType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEngineVersion :: Lens.Lens' Configuration (Lude.Maybe Lude.Text)
cEngineVersion = Lens.lens (engineVersion :: Configuration -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: Configuration)
{-# DEPRECATED cEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Required. The ARN of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cARN :: Lens.Lens' Configuration (Lude.Maybe Lude.Text)
cARN = Lens.lens (arn :: Configuration -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Configuration)
{-# DEPRECATED cARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Required. The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLatestRevision :: Lens.Lens' Configuration (Lude.Maybe ConfigurationRevision)
cLatestRevision = Lens.lens (latestRevision :: Configuration -> Lude.Maybe ConfigurationRevision) (\s a -> s {latestRevision = a} :: Configuration)
{-# DEPRECATED cLatestRevision "Use generic-lens or generic-optics with 'latestRevision' instead." #-}

-- | Required. The date and time of the configuration revision.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreated :: Lens.Lens' Configuration (Lude.Maybe Lude.Timestamp)
cCreated = Lens.lens (created :: Configuration -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Configuration)
{-# DEPRECATED cCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAuthenticationStrategy :: Lens.Lens' Configuration (Lude.Maybe AuthenticationStrategy)
cAuthenticationStrategy = Lens.lens (authenticationStrategy :: Configuration -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: Configuration)
{-# DEPRECATED cAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Configuration (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Configuration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Configuration)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Configuration (Lude.Maybe Lude.Text)
cId = Lens.lens (id :: Configuration -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Configuration)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Required. The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Configuration (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: Configuration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Configuration)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEngineType :: Lens.Lens' Configuration (Lude.Maybe EngineType)
cEngineType = Lens.lens (engineType :: Configuration -> Lude.Maybe EngineType) (\s a -> s {engineType = a} :: Configuration)
{-# DEPRECATED cEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The list of all tags associated with this configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Configuration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cTags = Lens.lens (tags :: Configuration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Configuration)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Configuration where
  parseJSON =
    Lude.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Lude.<$> (x Lude..:? "engineVersion")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "latestRevision")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "authenticationStrategy")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "engineType")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
