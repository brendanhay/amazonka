{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> from an input.
module Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
  ( -- * Creating a request
    DeleteApplicationInputProcessingConfiguration (..),
    mkDeleteApplicationInputProcessingConfiguration,

    -- ** Request lenses
    daipcApplicationName,
    daipcCurrentApplicationVersionId,
    daipcInputId,

    -- * Destructuring the response
    DeleteApplicationInputProcessingConfigurationResponse (..),
    mkDeleteApplicationInputProcessingConfigurationResponse,

    -- ** Response lenses
    daipcrsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApplicationInputProcessingConfiguration' smart constructor.
data DeleteApplicationInputProcessingConfiguration = DeleteApplicationInputProcessingConfiguration'
  { applicationName ::
      Lude.Text,
    currentApplicationVersionId ::
      Lude.Natural,
    inputId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteApplicationInputProcessingConfiguration' with the minimum fields required to make a request.
--
-- * 'applicationName' - The Kinesis Analytics application name.
-- * 'currentApplicationVersionId' - The version ID of the Kinesis Analytics application.
-- * 'inputId' - The ID of the input configuration from which to delete the input processing configuration. You can get a list of the input IDs for an application by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
mkDeleteApplicationInputProcessingConfiguration ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'inputId'
  Lude.Text ->
  DeleteApplicationInputProcessingConfiguration
mkDeleteApplicationInputProcessingConfiguration
  pApplicationName_
  pCurrentApplicationVersionId_
  pInputId_ =
    DeleteApplicationInputProcessingConfiguration'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        inputId = pInputId_
      }

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcApplicationName :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Lude.Text
daipcApplicationName = Lens.lens (applicationName :: DeleteApplicationInputProcessingConfiguration -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplicationInputProcessingConfiguration)
{-# DEPRECATED daipcApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Lude.Natural
daipcCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: DeleteApplicationInputProcessingConfiguration -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: DeleteApplicationInputProcessingConfiguration)
{-# DEPRECATED daipcCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The ID of the input configuration from which to delete the input processing configuration. You can get a list of the input IDs for an application by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcInputId :: Lens.Lens' DeleteApplicationInputProcessingConfiguration Lude.Text
daipcInputId = Lens.lens (inputId :: DeleteApplicationInputProcessingConfiguration -> Lude.Text) (\s a -> s {inputId = a} :: DeleteApplicationInputProcessingConfiguration)
{-# DEPRECATED daipcInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

instance
  Lude.AWSRequest
    DeleteApplicationInputProcessingConfiguration
  where
  type
    Rs DeleteApplicationInputProcessingConfiguration =
      DeleteApplicationInputProcessingConfigurationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteApplicationInputProcessingConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DeleteApplicationInputProcessingConfiguration
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.DeleteApplicationInputProcessingConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplicationInputProcessingConfiguration where
  toJSON DeleteApplicationInputProcessingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("InputId" Lude..= inputId)
          ]
      )

instance Lude.ToPath DeleteApplicationInputProcessingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplicationInputProcessingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApplicationInputProcessingConfigurationResponse' smart constructor.
newtype DeleteApplicationInputProcessingConfigurationResponse = DeleteApplicationInputProcessingConfigurationResponse'
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

-- | Creates a value of 'DeleteApplicationInputProcessingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteApplicationInputProcessingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApplicationInputProcessingConfigurationResponse
mkDeleteApplicationInputProcessingConfigurationResponse
  pResponseStatus_ =
    DeleteApplicationInputProcessingConfigurationResponse'
      { responseStatus =
          pResponseStatus_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daipcrsResponseStatus :: Lens.Lens' DeleteApplicationInputProcessingConfigurationResponse Lude.Int
daipcrsResponseStatus = Lens.lens (responseStatus :: DeleteApplicationInputProcessingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApplicationInputProcessingConfigurationResponse)
{-# DEPRECATED daipcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
