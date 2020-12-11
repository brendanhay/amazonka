{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DescribeApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Kinesis Analytics application.
--
-- If you want to retrieve a list of all applications in your account, use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_ListApplications.html ListApplications> operation.
-- This operation requires permissions to perform the @kinesisanalytics:DescribeApplication@ action. You can use @DescribeApplication@ to get the current application versionId, which you need to call other operations such as @Update@ .
module Network.AWS.KinesisAnalytics.DescribeApplication
  ( -- * Creating a request
    DescribeApplication (..),
    mkDescribeApplication,

    -- ** Request lenses
    daApplicationName,

    -- * Destructuring the response
    DescribeApplicationResponse (..),
    mkDescribeApplicationResponse,

    -- ** Response lenses
    darsResponseStatus,
    darsApplicationDetail,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeApplication' smart constructor.
newtype DescribeApplication = DescribeApplication'
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

-- | Creates a value of 'DescribeApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of the application.
mkDescribeApplication ::
  -- | 'applicationName'
  Lude.Text ->
  DescribeApplication
mkDescribeApplication pApplicationName_ =
  DescribeApplication' {applicationName = pApplicationName_}

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationName :: Lens.Lens' DescribeApplication Lude.Text
daApplicationName = Lens.lens (applicationName :: DescribeApplication -> Lude.Text) (\s a -> s {applicationName = a} :: DescribeApplication)
{-# DEPRECATED daApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DescribeApplication where
  type Rs DescribeApplication = DescribeApplicationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeApplicationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ApplicationDetail")
      )

instance Lude.ToHeaders DescribeApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.DescribeApplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeApplication where
  toJSON DescribeApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ApplicationName" Lude..= applicationName)]
      )

instance Lude.ToPath DescribeApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApplication where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { responseStatus ::
      Lude.Int,
    applicationDetail ::
      ApplicationDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicationResponse' with the minimum fields required to make a request.
--
-- * 'applicationDetail' - Provides a description of the application, such as the application Amazon Resource Name (ARN), status, latest version, and input and output configuration details.
-- * 'responseStatus' - The response status code.
mkDescribeApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'applicationDetail'
  ApplicationDetail ->
  DescribeApplicationResponse
mkDescribeApplicationResponse pResponseStatus_ pApplicationDetail_ =
  DescribeApplicationResponse'
    { responseStatus = pResponseStatus_,
      applicationDetail = pApplicationDetail_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeApplicationResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeApplicationResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Provides a description of the application, such as the application Amazon Resource Name (ARN), status, latest version, and input and output configuration details.
--
-- /Note:/ Consider using 'applicationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsApplicationDetail :: Lens.Lens' DescribeApplicationResponse ApplicationDetail
darsApplicationDetail = Lens.lens (applicationDetail :: DescribeApplicationResponse -> ApplicationDetail) (\s a -> s {applicationDetail = a} :: DescribeApplicationResponse)
{-# DEPRECATED darsApplicationDetail "Use generic-lens or generic-optics with 'applicationDetail' instead." #-}
