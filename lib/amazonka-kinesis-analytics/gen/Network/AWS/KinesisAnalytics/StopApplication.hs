{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.StopApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the application from processing input data. You can stop an application only if it is in the running state. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the application state. After the application is stopped, Amazon Kinesis Analytics stops reading data from the input, the application stops processing data, and there is no output written to the destination.
--
-- This operation requires permissions to perform the @kinesisanalytics:StopApplication@ action.
module Network.AWS.KinesisAnalytics.StopApplication
  ( -- * Creating a request
    StopApplication (..),
    mkStopApplication,

    -- ** Request lenses
    sApplicationName,

    -- * Destructuring the response
    StopApplicationResponse (..),
    mkStopApplicationResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStopApplication' smart constructor.
newtype StopApplication = StopApplication'
  { applicationName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of the running application to stop.
mkStopApplication ::
  -- | 'applicationName'
  Lude.Text ->
  StopApplication
mkStopApplication pApplicationName_ =
  StopApplication' {applicationName = pApplicationName_}

-- | Name of the running application to stop.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplicationName :: Lens.Lens' StopApplication Lude.Text
sApplicationName = Lens.lens (applicationName :: StopApplication -> Lude.Text) (\s a -> s {applicationName = a} :: StopApplication)
{-# DEPRECATED sApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest StopApplication where
  type Rs StopApplication = StopApplicationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopApplicationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("KinesisAnalytics_20150814.StopApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopApplication where
  toJSON StopApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ApplicationName" Lude..= applicationName)]
      )

instance Lude.ToPath StopApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery StopApplication where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkStopApplicationResponse' smart constructor.
newtype StopApplicationResponse = StopApplicationResponse'
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
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopApplicationResponse
mkStopApplicationResponse pResponseStatus_ =
  StopApplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopApplicationResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopApplicationResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
