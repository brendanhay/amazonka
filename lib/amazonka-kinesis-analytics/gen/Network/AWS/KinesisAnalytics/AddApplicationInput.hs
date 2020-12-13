{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a streaming source to your Amazon Kinesis application. For conceptual information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- You can add a streaming source either when you create an application or you can use this operation to add a streaming source after you create an application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_CreateApplication.html CreateApplication> .
-- Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationInput@ action.
module Network.AWS.KinesisAnalytics.AddApplicationInput
  ( -- * Creating a request
    AddApplicationInput (..),
    mkAddApplicationInput,

    -- ** Request lenses
    aaiCurrentApplicationVersionId,
    aaiInput,
    aaiApplicationName,

    -- * Destructuring the response
    AddApplicationInputResponse (..),
    mkAddApplicationInputResponse,

    -- ** Response lenses
    aairsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAddApplicationInput' smart constructor.
data AddApplicationInput = AddApplicationInput'
  { -- | Current version of your Amazon Kinesis Analytics application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
    currentApplicationVersionId :: Lude.Natural,
    -- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_Input.html Input> to add.
    input :: Input,
    -- | Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationInput' with the minimum fields required to make a request.
--
-- * 'currentApplicationVersionId' - Current version of your Amazon Kinesis Analytics application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
-- * 'input' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_Input.html Input> to add.
-- * 'applicationName' - Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
mkAddApplicationInput ::
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'input'
  Input ->
  -- | 'applicationName'
  Lude.Text ->
  AddApplicationInput
mkAddApplicationInput
  pCurrentApplicationVersionId_
  pInput_
  pApplicationName_ =
    AddApplicationInput'
      { currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        input = pInput_,
        applicationName = pApplicationName_
      }

-- | Current version of your Amazon Kinesis Analytics application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaiCurrentApplicationVersionId :: Lens.Lens' AddApplicationInput Lude.Natural
aaiCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: AddApplicationInput -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: AddApplicationInput)
{-# DEPRECATED aaiCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_Input.html Input> to add.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaiInput :: Lens.Lens' AddApplicationInput Input
aaiInput = Lens.lens (input :: AddApplicationInput -> Input) (\s a -> s {input = a} :: AddApplicationInput)
{-# DEPRECATED aaiInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaiApplicationName :: Lens.Lens' AddApplicationInput Lude.Text
aaiApplicationName = Lens.lens (applicationName :: AddApplicationInput -> Lude.Text) (\s a -> s {applicationName = a} :: AddApplicationInput)
{-# DEPRECATED aaiApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest AddApplicationInput where
  type Rs AddApplicationInput = AddApplicationInputResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddApplicationInputResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddApplicationInput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.AddApplicationInput" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddApplicationInput where
  toJSON AddApplicationInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("Input" Lude..= input),
            Lude.Just ("ApplicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath AddApplicationInput where
  toPath = Lude.const "/"

instance Lude.ToQuery AddApplicationInput where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkAddApplicationInputResponse' smart constructor.
newtype AddApplicationInputResponse = AddApplicationInputResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationInputResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddApplicationInputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddApplicationInputResponse
mkAddApplicationInputResponse pResponseStatus_ =
  AddApplicationInputResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aairsResponseStatus :: Lens.Lens' AddApplicationInputResponse Lude.Int
aairsResponseStatus = Lens.lens (responseStatus :: AddApplicationInputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddApplicationInputResponse)
{-# DEPRECATED aairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
