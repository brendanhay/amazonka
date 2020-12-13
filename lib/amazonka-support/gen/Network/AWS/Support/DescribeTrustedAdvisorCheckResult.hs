{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results of the AWS Trusted Advisor check that has the specified check ID. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- The response contains a 'TrustedAdvisorCheckResult' object, which contains these three objects:
--
--     * 'TrustedAdvisorCategorySpecificSummary'
--
--
--     * 'TrustedAdvisorResourceDetail'
--
--
--     * 'TrustedAdvisorResourcesSummary'
--
--
-- In addition, the response contains these fields:
--
--     * __status__ - The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
--
--     * __timestamp__ - The time of the last refresh of the check.
--
--
--     * __checkId__ - The unique identifier for the check.
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
  ( -- * Creating a request
    DescribeTrustedAdvisorCheckResult (..),
    mkDescribeTrustedAdvisorCheckResult,

    -- ** Request lenses
    dtacrCheckId,
    dtacrLanguage,

    -- * Destructuring the response
    DescribeTrustedAdvisorCheckResultResponse (..),
    mkDescribeTrustedAdvisorCheckResultResponse,

    -- ** Response lenses
    dtacrrsResult,
    dtacrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- |
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckResult' smart constructor.
data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Lude.Text,
    -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorCheckResult' with the minimum fields required to make a request.
--
-- * 'checkId' - The unique identifier for the Trusted Advisor check.
-- * 'language' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
mkDescribeTrustedAdvisorCheckResult ::
  -- | 'checkId'
  Lude.Text ->
  DescribeTrustedAdvisorCheckResult
mkDescribeTrustedAdvisorCheckResult pCheckId_ =
  DescribeTrustedAdvisorCheckResult'
    { checkId = pCheckId_,
      language = Lude.Nothing
    }

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrCheckId :: Lens.Lens' DescribeTrustedAdvisorCheckResult Lude.Text
dtacrCheckId = Lens.lens (checkId :: DescribeTrustedAdvisorCheckResult -> Lude.Text) (\s a -> s {checkId = a} :: DescribeTrustedAdvisorCheckResult)
{-# DEPRECATED dtacrCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrLanguage :: Lens.Lens' DescribeTrustedAdvisorCheckResult (Lude.Maybe Lude.Text)
dtacrLanguage = Lens.lens (language :: DescribeTrustedAdvisorCheckResult -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: DescribeTrustedAdvisorCheckResult)
{-# DEPRECATED dtacrLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Lude.AWSRequest DescribeTrustedAdvisorCheckResult where
  type
    Rs DescribeTrustedAdvisorCheckResult =
      DescribeTrustedAdvisorCheckResultResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckResultResponse'
            Lude.<$> (x Lude..?> "result") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrustedAdvisorCheckResult where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckResult" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrustedAdvisorCheckResult where
  toJSON DescribeTrustedAdvisorCheckResult' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("checkId" Lude..= checkId),
            ("language" Lude..=) Lude.<$> language
          ]
      )

instance Lude.ToPath DescribeTrustedAdvisorCheckResult where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrustedAdvisorCheckResult where
  toQuery = Lude.const Lude.mempty

-- | The result of the Trusted Advisor check returned by the 'DescribeTrustedAdvisorCheckResult' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckResultResponse' smart constructor.
data DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'
  { -- | The detailed results of the Trusted Advisor check.
    result :: Lude.Maybe TrustedAdvisorCheckResult,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorCheckResultResponse' with the minimum fields required to make a request.
--
-- * 'result' - The detailed results of the Trusted Advisor check.
-- * 'responseStatus' - The response status code.
mkDescribeTrustedAdvisorCheckResultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrustedAdvisorCheckResultResponse
mkDescribeTrustedAdvisorCheckResultResponse pResponseStatus_ =
  DescribeTrustedAdvisorCheckResultResponse'
    { result = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The detailed results of the Trusted Advisor check.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrrsResult :: Lens.Lens' DescribeTrustedAdvisorCheckResultResponse (Lude.Maybe TrustedAdvisorCheckResult)
dtacrrsResult = Lens.lens (result :: DescribeTrustedAdvisorCheckResultResponse -> Lude.Maybe TrustedAdvisorCheckResult) (\s a -> s {result = a} :: DescribeTrustedAdvisorCheckResultResponse)
{-# DEPRECATED dtacrrsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorCheckResultResponse Lude.Int
dtacrrsResponseStatus = Lens.lens (responseStatus :: DescribeTrustedAdvisorCheckResultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrustedAdvisorCheckResultResponse)
{-# DEPRECATED dtacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
