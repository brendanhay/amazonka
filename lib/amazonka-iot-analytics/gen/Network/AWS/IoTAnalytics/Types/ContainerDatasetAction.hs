{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
  ( ContainerDatasetAction (..),

    -- * Smart constructor
    mkContainerDatasetAction,

    -- * Lenses
    cdaImage,
    cdaResourceConfiguration,
    cdaVariables,
    cdaExecutionRoleARN,
  )
where

import Network.AWS.IoTAnalytics.Types.ResourceConfiguration
import Network.AWS.IoTAnalytics.Types.Variable
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information required to run the @containerAction@ to produce dataset contents.
--
-- /See:/ 'mkContainerDatasetAction' smart constructor.
data ContainerDatasetAction = ContainerDatasetAction'
  { -- | The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
    image :: Lude.Text,
    -- | Configuration of the resource that executes the @containerAction@ .
    resourceConfiguration :: ResourceConfiguration,
    -- | The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
    variables :: Lude.Maybe [Variable],
    -- | The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
    executionRoleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerDatasetAction' with the minimum fields required to make a request.
--
-- * 'image' - The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
-- * 'resourceConfiguration' - Configuration of the resource that executes the @containerAction@ .
-- * 'variables' - The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
-- * 'executionRoleARN' - The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
mkContainerDatasetAction ::
  -- | 'image'
  Lude.Text ->
  -- | 'resourceConfiguration'
  ResourceConfiguration ->
  -- | 'executionRoleARN'
  Lude.Text ->
  ContainerDatasetAction
mkContainerDatasetAction
  pImage_
  pResourceConfiguration_
  pExecutionRoleARN_ =
    ContainerDatasetAction'
      { image = pImage_,
        resourceConfiguration = pResourceConfiguration_,
        variables = Lude.Nothing,
        executionRoleARN = pExecutionRoleARN_
      }

-- | The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaImage :: Lens.Lens' ContainerDatasetAction Lude.Text
cdaImage = Lens.lens (image :: ContainerDatasetAction -> Lude.Text) (\s a -> s {image = a} :: ContainerDatasetAction)
{-# DEPRECATED cdaImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | Configuration of the resource that executes the @containerAction@ .
--
-- /Note:/ Consider using 'resourceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaResourceConfiguration :: Lens.Lens' ContainerDatasetAction ResourceConfiguration
cdaResourceConfiguration = Lens.lens (resourceConfiguration :: ContainerDatasetAction -> ResourceConfiguration) (\s a -> s {resourceConfiguration = a} :: ContainerDatasetAction)
{-# DEPRECATED cdaResourceConfiguration "Use generic-lens or generic-optics with 'resourceConfiguration' instead." #-}

-- | The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaVariables :: Lens.Lens' ContainerDatasetAction (Lude.Maybe [Variable])
cdaVariables = Lens.lens (variables :: ContainerDatasetAction -> Lude.Maybe [Variable]) (\s a -> s {variables = a} :: ContainerDatasetAction)
{-# DEPRECATED cdaVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

-- | The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaExecutionRoleARN :: Lens.Lens' ContainerDatasetAction Lude.Text
cdaExecutionRoleARN = Lens.lens (executionRoleARN :: ContainerDatasetAction -> Lude.Text) (\s a -> s {executionRoleARN = a} :: ContainerDatasetAction)
{-# DEPRECATED cdaExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

instance Lude.FromJSON ContainerDatasetAction where
  parseJSON =
    Lude.withObject
      "ContainerDatasetAction"
      ( \x ->
          ContainerDatasetAction'
            Lude.<$> (x Lude..: "image")
            Lude.<*> (x Lude..: "resourceConfiguration")
            Lude.<*> (x Lude..:? "variables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "executionRoleArn")
      )

instance Lude.ToJSON ContainerDatasetAction where
  toJSON ContainerDatasetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("image" Lude..= image),
            Lude.Just ("resourceConfiguration" Lude..= resourceConfiguration),
            ("variables" Lude..=) Lude.<$> variables,
            Lude.Just ("executionRoleArn" Lude..= executionRoleARN)
          ]
      )
