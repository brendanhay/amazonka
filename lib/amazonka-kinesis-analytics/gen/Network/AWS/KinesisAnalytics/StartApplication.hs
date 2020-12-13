{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.StartApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.
--
-- After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.
-- The application status must be @READY@ for you to start an application. You can get the application status in the console or using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
-- After you start the application, you can stop the application from processing the input by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_StopApplication.html StopApplication> operation.
-- This operation requires permissions to perform the @kinesisanalytics:StartApplication@ action.
module Network.AWS.KinesisAnalytics.StartApplication
  ( -- * Creating a request
    StartApplication (..),
    mkStartApplication,

    -- ** Request lenses
    sApplicationName,
    sInputConfigurations,

    -- * Destructuring the response
    StartApplicationResponse (..),
    mkStartApplicationResponse,

    -- ** Response lenses
    sarsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStartApplication' smart constructor.
data StartApplication = StartApplication'
  { -- | Name of the application.
    applicationName :: Lude.Text,
    -- | Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
    inputConfigurations :: [InputConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of the application.
-- * 'inputConfigurations' - Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
mkStartApplication ::
  -- | 'applicationName'
  Lude.Text ->
  StartApplication
mkStartApplication pApplicationName_ =
  StartApplication'
    { applicationName = pApplicationName_,
      inputConfigurations = Lude.mempty
    }

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplicationName :: Lens.Lens' StartApplication Lude.Text
sApplicationName = Lens.lens (applicationName :: StartApplication -> Lude.Text) (\s a -> s {applicationName = a} :: StartApplication)
{-# DEPRECATED sApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
--
-- /Note:/ Consider using 'inputConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInputConfigurations :: Lens.Lens' StartApplication [InputConfiguration]
sInputConfigurations = Lens.lens (inputConfigurations :: StartApplication -> [InputConfiguration]) (\s a -> s {inputConfigurations = a} :: StartApplication)
{-# DEPRECATED sInputConfigurations "Use generic-lens or generic-optics with 'inputConfigurations' instead." #-}

instance Lude.AWSRequest StartApplication where
  type Rs StartApplication = StartApplicationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartApplicationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("KinesisAnalytics_20150814.StartApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartApplication where
  toJSON StartApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just ("InputConfigurations" Lude..= inputConfigurations)
          ]
      )

instance Lude.ToPath StartApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery StartApplication where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkStartApplicationResponse' smart constructor.
newtype StartApplicationResponse = StartApplicationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartApplicationResponse
mkStartApplicationResponse pResponseStatus_ =
  StartApplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarsResponseStatus :: Lens.Lens' StartApplicationResponse Lude.Int
sarsResponseStatus = Lens.lens (responseStatus :: StartApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartApplicationResponse)
{-# DEPRECATED sarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
