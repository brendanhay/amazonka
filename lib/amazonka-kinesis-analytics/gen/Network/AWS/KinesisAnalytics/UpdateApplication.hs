{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration.
--
-- Note that Amazon Kinesis Analytics updates the @CurrentApplicationVersionId@ each time you update your application.
-- This operation requires permission for the @kinesisanalytics:UpdateApplication@ action.
module Network.AWS.KinesisAnalytics.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uaApplicationName,
    uaCurrentApplicationVersionId,
    uaApplicationUpdate,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,

    -- ** Response lenses
    uarsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { applicationName ::
      Lude.Text,
    currentApplicationVersionId :: Lude.Natural,
    applicationUpdate :: ApplicationUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of the Amazon Kinesis Analytics application to update.
-- * 'applicationUpdate' - Describes application updates.
-- * 'currentApplicationVersionId' - The current application version ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
mkUpdateApplication ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'applicationUpdate'
  ApplicationUpdate ->
  UpdateApplication
mkUpdateApplication
  pApplicationName_
  pCurrentApplicationVersionId_
  pApplicationUpdate_ =
    UpdateApplication'
      { applicationName = pApplicationName_,
        currentApplicationVersionId = pCurrentApplicationVersionId_,
        applicationUpdate = pApplicationUpdate_
      }

-- | Name of the Amazon Kinesis Analytics application to update.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication Lude.Text
uaApplicationName = Lens.lens (applicationName :: UpdateApplication -> Lude.Text) (\s a -> s {applicationName = a} :: UpdateApplication)
{-# DEPRECATED uaApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The current application version ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCurrentApplicationVersionId :: Lens.Lens' UpdateApplication Lude.Natural
uaCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: UpdateApplication -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: UpdateApplication)
{-# DEPRECATED uaCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | Describes application updates.
--
-- /Note:/ Consider using 'applicationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationUpdate :: Lens.Lens' UpdateApplication ApplicationUpdate
uaApplicationUpdate = Lens.lens (applicationUpdate :: UpdateApplication -> ApplicationUpdate) (\s a -> s {applicationUpdate = a} :: UpdateApplication)
{-# DEPRECATED uaApplicationUpdate "Use generic-lens or generic-optics with 'applicationUpdate' instead." #-}

instance Lude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateApplicationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("KinesisAnalytics_20150814.UpdateApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("ApplicationUpdate" Lude..= applicationUpdate)
          ]
      )

instance Lude.ToPath UpdateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
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

-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateApplicationResponse
mkUpdateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateApplicationResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
