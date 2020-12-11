-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FirelensConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FirelensConfiguration
  ( FirelensConfiguration (..),

    -- * Smart constructor
    mkFirelensConfiguration,

    -- * Lenses
    fcOptions,
    fcType,
  )
where

import Network.AWS.ECS.Types.FirelensConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFirelensConfiguration' smart constructor.
data FirelensConfiguration = FirelensConfiguration'
  { options ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    type' :: FirelensConfigurationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FirelensConfiguration' with the minimum fields required to make a request.
--
-- * 'options' - The options to use when configuring the log router. This field is optional and can be used to specify a custom configuration file or to add additional metadata, such as the task, task definition, cluster, and container instance details to the log event. If specified, the syntax to use is @"options":{"enable-ecs-log-metadata":"true|false","config-file-type:"s3|file","config-file-value":"arn:aws:s3:::mybucket/fluent.conf|filepath"}@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a Task Definition that Uses a FireLens Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'type'' - The log router to use. The valid values are @fluentd@ or @fluentbit@ .
mkFirelensConfiguration ::
  -- | 'type''
  FirelensConfigurationType ->
  FirelensConfiguration
mkFirelensConfiguration pType_ =
  FirelensConfiguration' {options = Lude.Nothing, type' = pType_}

-- | The options to use when configuring the log router. This field is optional and can be used to specify a custom configuration file or to add additional metadata, such as the task, task definition, cluster, and container instance details to the log event. If specified, the syntax to use is @"options":{"enable-ecs-log-metadata":"true|false","config-file-type:"s3|file","config-file-value":"arn:aws:s3:::mybucket/fluent.conf|filepath"}@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a Task Definition that Uses a FireLens Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcOptions :: Lens.Lens' FirelensConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
fcOptions = Lens.lens (options :: FirelensConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {options = a} :: FirelensConfiguration)
{-# DEPRECATED fcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The log router to use. The valid values are @fluentd@ or @fluentbit@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcType :: Lens.Lens' FirelensConfiguration FirelensConfigurationType
fcType = Lens.lens (type' :: FirelensConfiguration -> FirelensConfigurationType) (\s a -> s {type' = a} :: FirelensConfiguration)
{-# DEPRECATED fcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON FirelensConfiguration where
  parseJSON =
    Lude.withObject
      "FirelensConfiguration"
      ( \x ->
          FirelensConfiguration'
            Lude.<$> (x Lude..:? "options" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON FirelensConfiguration where
  toJSON FirelensConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("options" Lude..=) Lude.<$> options,
            Lude.Just ("type" Lude..= type')
          ]
      )
