{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectDominantLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text. For a list of languages that Amazon Comprehend can detect, see <https://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages> .
module Network.AWS.Comprehend.DetectDominantLanguage
  ( -- * Creating a request
    DetectDominantLanguage (..),
    mkDetectDominantLanguage,

    -- ** Request lenses
    ddlText,

    -- * Destructuring the response
    DetectDominantLanguageResponse (..),
    mkDetectDominantLanguageResponse,

    -- ** Response lenses
    ddlrsLanguages,
    ddlrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetectDominantLanguage' smart constructor.
newtype DetectDominantLanguage = DetectDominantLanguage'
  { text ::
      Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectDominantLanguage' with the minimum fields required to make a request.
--
-- * 'text' - A UTF-8 text string. Each string should contain at least 20 characters and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkDetectDominantLanguage ::
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  DetectDominantLanguage
mkDetectDominantLanguage pText_ =
  DetectDominantLanguage' {text = pText_}

-- | A UTF-8 text string. Each string should contain at least 20 characters and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlText :: Lens.Lens' DetectDominantLanguage (Lude.Sensitive Lude.Text)
ddlText = Lens.lens (text :: DetectDominantLanguage -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: DetectDominantLanguage)
{-# DEPRECATED ddlText "Use generic-lens or generic-optics with 'text' instead." #-}

instance Lude.AWSRequest DetectDominantLanguage where
  type Rs DetectDominantLanguage = DetectDominantLanguageResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetectDominantLanguageResponse'
            Lude.<$> (x Lude..?> "Languages" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetectDominantLanguage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DetectDominantLanguage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetectDominantLanguage where
  toJSON DetectDominantLanguage' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Text" Lude..= text)])

instance Lude.ToPath DetectDominantLanguage where
  toPath = Lude.const "/"

instance Lude.ToQuery DetectDominantLanguage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetectDominantLanguageResponse' smart constructor.
data DetectDominantLanguageResponse = DetectDominantLanguageResponse'
  { languages ::
      Lude.Maybe [DominantLanguage],
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

-- | Creates a value of 'DetectDominantLanguageResponse' with the minimum fields required to make a request.
--
-- * 'languages' - The languages that Amazon Comprehend detected in the input text. For each language, the response returns the RFC 5646 language code and the level of confidence that Amazon Comprehend has in the accuracy of its inference. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
-- * 'responseStatus' - The response status code.
mkDetectDominantLanguageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetectDominantLanguageResponse
mkDetectDominantLanguageResponse pResponseStatus_ =
  DetectDominantLanguageResponse'
    { languages = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The languages that Amazon Comprehend detected in the input text. For each language, the response returns the RFC 5646 language code and the level of confidence that Amazon Comprehend has in the accuracy of its inference. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
--
-- /Note:/ Consider using 'languages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlrsLanguages :: Lens.Lens' DetectDominantLanguageResponse (Lude.Maybe [DominantLanguage])
ddlrsLanguages = Lens.lens (languages :: DetectDominantLanguageResponse -> Lude.Maybe [DominantLanguage]) (\s a -> s {languages = a} :: DetectDominantLanguageResponse)
{-# DEPRECATED ddlrsLanguages "Use generic-lens or generic-optics with 'languages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlrsResponseStatus :: Lens.Lens' DetectDominantLanguageResponse Lude.Int
ddlrsResponseStatus = Lens.lens (responseStatus :: DetectDominantLanguageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetectDominantLanguageResponse)
{-# DEPRECATED ddlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
