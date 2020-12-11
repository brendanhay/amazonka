{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DetectText
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects text in the input image and converts it into machine-readable text.
--
-- Pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, you must pass it as a reference to an image in an Amazon S3 bucket. For the AWS CLI, passing image bytes is not supported. The image must be either a .png or .jpeg formatted file.
-- The @DetectText@ operation returns text in an array of 'TextDetection' elements, @TextDetections@ . Each @TextDetection@ element provides information about a single word or line of text that was detected in the image.
-- A word is one or more ISO basic latin script characters that are not separated by spaces. @DetectText@ can detect up to 50 words in an image.
-- A line is a string of equally spaced words. A line isn't necessarily a complete sentence. For example, a driver's license number is detected as a line. A line ends when there is no aligned text after it. Also, a line ends when there is a large gap between words, relative to the length of the words. This means, depending on the gap between words, Amazon Rekognition may detect multiple lines in text aligned in the same direction. Periods don't represent the end of a line. If a sentence spans multiple lines, the @DetectText@ operation returns multiple lines.
-- To determine whether a @TextDetection@ element is a line of text or a word, use the @TextDetection@ object @Type@ field.
-- To be detected, text must be within +/- 90 degrees orientation of the horizontal axis.
-- For more information, see DetectText in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.DetectText
  ( -- * Creating a request
    DetectText (..),
    mkDetectText,

    -- ** Request lenses
    dtFilters,
    dtImage,

    -- * Destructuring the response
    DetectTextResponse (..),
    mkDetectTextResponse,

    -- ** Response lenses
    dtrsTextDetections,
    dtrsTextModelVersion,
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectText' smart constructor.
data DetectText = DetectText'
  { filters ::
      Lude.Maybe DetectTextFilters,
    image :: Image
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectText' with the minimum fields required to make a request.
--
-- * 'filters' - Optional parameters that let you set the criteria that the text must meet to be included in your response.
-- * 'image' - The input image as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
mkDetectText ::
  -- | 'image'
  Image ->
  DetectText
mkDetectText pImage_ =
  DetectText' {filters = Lude.Nothing, image = pImage_}

-- | Optional parameters that let you set the criteria that the text must meet to be included in your response.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DetectText (Lude.Maybe DetectTextFilters)
dtFilters = Lens.lens (filters :: DetectText -> Lude.Maybe DetectTextFilters) (\s a -> s {filters = a} :: DetectText)
{-# DEPRECATED dtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The input image as base64-encoded bytes or an Amazon S3 object. If you use the AWS CLI to call Amazon Rekognition operations, you can't pass image bytes.
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtImage :: Lens.Lens' DetectText Image
dtImage = Lens.lens (image :: DetectText -> Image) (\s a -> s {image = a} :: DetectText)
{-# DEPRECATED dtImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.AWSRequest DetectText where
  type Rs DetectText = DetectTextResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectTextResponse'
            Lude.<$> (x Lude..?> "TextDetections" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TextModelVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectText where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DetectText" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectText where
  toJSON DetectText' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            Lude.Just ("Image" Lude..= image)
          ]
      )

instance Lude.ToPath DetectText where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectText where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectTextResponse' smart constructor.
data DetectTextResponse = DetectTextResponse'
  { textDetections ::
      Lude.Maybe [TextDetection],
    textModelVersion :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectTextResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'textDetections' - An array of text that was detected in the input image.
-- * 'textModelVersion' - The model version used to detect text.
mkDetectTextResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectTextResponse
mkDetectTextResponse pResponseStatus_ =
  DetectTextResponse'
    { textDetections = Lude.Nothing,
      textModelVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of text that was detected in the input image.
--
-- /Note:/ Consider using 'textDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTextDetections :: Lens.Lens' DetectTextResponse (Lude.Maybe [TextDetection])
dtrsTextDetections = Lens.lens (textDetections :: DetectTextResponse -> Lude.Maybe [TextDetection]) (\s a -> s {textDetections = a} :: DetectTextResponse)
{-# DEPRECATED dtrsTextDetections "Use generic-lens or generic-optics with 'textDetections' instead." #-}

-- | The model version used to detect text.
--
-- /Note:/ Consider using 'textModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTextModelVersion :: Lens.Lens' DetectTextResponse (Lude.Maybe Lude.Text)
dtrsTextModelVersion = Lens.lens (textModelVersion :: DetectTextResponse -> Lude.Maybe Lude.Text) (\s a -> s {textModelVersion = a} :: DetectTextResponse)
{-# DEPRECATED dtrsTextModelVersion "Use generic-lens or generic-optics with 'textModelVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DetectTextResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DetectTextResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectTextResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
