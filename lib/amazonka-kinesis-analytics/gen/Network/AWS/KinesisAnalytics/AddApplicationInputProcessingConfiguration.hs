{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to an application. An input processor preprocesses records on the input stream before the application's SQL code executes. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
module Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
  ( -- * Creating a request
    AddApplicationInputProcessingConfiguration (..),
    mkAddApplicationInputProcessingConfiguration,

    -- ** Request lenses
    aaipcApplicationName,
    aaipcCurrentApplicationVersionId,
    aaipcInputId,
    aaipcInputProcessingConfiguration,

    -- * Destructuring the response
    AddApplicationInputProcessingConfigurationResponse (..),
    mkAddApplicationInputProcessingConfigurationResponse,

    -- ** Response lenses
    aaipcrsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddApplicationInputProcessingConfiguration' smart constructor.
data AddApplicationInputProcessingConfiguration = AddApplicationInputProcessingConfiguration'
  { applicationName ::
      Lude.Text,
    currentApplicationVersionId ::
      Lude.Natural,
    inputId ::
      Lude.Text,
    inputProcessingConfiguration ::
      InputProcessingConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationInputProcessingConfiguration' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of the application to which you want to add the input processing configuration.
-- * 'currentApplicationVersionId' - Version of the application to which you want to add the input processing configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
-- * 'inputId' - The ID of the input configuration to add the input processing configuration to. You can get a list of the input IDs for an application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
-- * 'inputProcessingConfiguration' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to add to the application.
mkAddApplicationInputProcessingConfiguration ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'inputId'
  Lude.Text ->
  -- | 'inputProcessingConfiguration'
  InputProcessingConfiguration ->
  AddApplicationInputProcessingConfiguration
mkAddApplicationInputProcessingConfiguration
  pApplicationName_
  pCurrentApplicationVersionId_
  pInputId_
  pInputProcessingConfiguration_ =
    AddApplicationInputProcessingConfiguration'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        inputId = pInputId_,
        inputProcessingConfiguration =
          pInputProcessingConfiguration_
      }

-- | Name of the application to which you want to add the input processing configuration.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcApplicationName :: Lens.Lens' AddApplicationInputProcessingConfiguration Lude.Text
aaipcApplicationName = Lens.lens (applicationName :: AddApplicationInputProcessingConfiguration -> Lude.Text) (\s a -> s {applicationName = a} :: AddApplicationInputProcessingConfiguration)
{-# DEPRECATED aaipcApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Version of the application to which you want to add the input processing configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcCurrentApplicationVersionId :: Lens.Lens' AddApplicationInputProcessingConfiguration Lude.Natural
aaipcCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: AddApplicationInputProcessingConfiguration -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: AddApplicationInputProcessingConfiguration)
{-# DEPRECATED aaipcCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The ID of the input configuration to add the input processing configuration to. You can get a list of the input IDs for an application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcInputId :: Lens.Lens' AddApplicationInputProcessingConfiguration Lude.Text
aaipcInputId = Lens.lens (inputId :: AddApplicationInputProcessingConfiguration -> Lude.Text) (\s a -> s {inputId = a} :: AddApplicationInputProcessingConfiguration)
{-# DEPRECATED aaipcInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to add to the application.
--
-- /Note:/ Consider using 'inputProcessingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcInputProcessingConfiguration :: Lens.Lens' AddApplicationInputProcessingConfiguration InputProcessingConfiguration
aaipcInputProcessingConfiguration = Lens.lens (inputProcessingConfiguration :: AddApplicationInputProcessingConfiguration -> InputProcessingConfiguration) (\s a -> s {inputProcessingConfiguration = a} :: AddApplicationInputProcessingConfiguration)
{-# DEPRECATED aaipcInputProcessingConfiguration "Use generic-lens or generic-optics with 'inputProcessingConfiguration' instead." #-}

instance Lude.AWSRequest AddApplicationInputProcessingConfiguration where
  type
    Rs AddApplicationInputProcessingConfiguration =
      AddApplicationInputProcessingConfigurationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddApplicationInputProcessingConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddApplicationInputProcessingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.AddApplicationInputProcessingConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddApplicationInputProcessingConfiguration where
  toJSON AddApplicationInputProcessingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("InputId" Lude..= inputId),
            Lude.Just
              ( "InputProcessingConfiguration"
                  Lude..= inputProcessingConfiguration
              )
          ]
      )

instance Lude.ToPath AddApplicationInputProcessingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery AddApplicationInputProcessingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddApplicationInputProcessingConfigurationResponse' smart constructor.
newtype AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'AddApplicationInputProcessingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddApplicationInputProcessingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddApplicationInputProcessingConfigurationResponse
mkAddApplicationInputProcessingConfigurationResponse
  pResponseStatus_ =
    AddApplicationInputProcessingConfigurationResponse'
      { responseStatus =
          pResponseStatus_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcrsResponseStatus :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse Lude.Int
aaipcrsResponseStatus = Lens.lens (responseStatus :: AddApplicationInputProcessingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddApplicationInputProcessingConfigurationResponse)
{-# DEPRECATED aaipcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
