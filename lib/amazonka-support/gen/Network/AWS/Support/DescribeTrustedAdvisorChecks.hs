{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all available AWS Trusted Advisor checks, including the name, ID, category, description, and metadata. You must specify a language code. The AWS Support API currently supports English ("en") and Japanese ("ja"). The response contains a 'TrustedAdvisorCheckDescription' object for each check. You must set the AWS Region to us-east-1.
module Network.AWS.Support.DescribeTrustedAdvisorChecks
  ( -- * Creating a request
    DescribeTrustedAdvisorChecks (..),
    mkDescribeTrustedAdvisorChecks,

    -- ** Request lenses
    dtacLanguage,

    -- * Destructuring the response
    DescribeTrustedAdvisorChecksResponse (..),
    mkDescribeTrustedAdvisorChecksResponse,

    -- ** Response lenses
    dtacrsChecks,
    dtacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeTrustedAdvisorChecks' smart constructor.
newtype DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorChecks' with the minimum fields required to make a request.
--
-- * 'language' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
mkDescribeTrustedAdvisorChecks ::
  -- | 'language'
  Lude.Text ->
  DescribeTrustedAdvisorChecks
mkDescribeTrustedAdvisorChecks pLanguage_ =
  DescribeTrustedAdvisorChecks' {language = pLanguage_}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacLanguage :: Lens.Lens' DescribeTrustedAdvisorChecks Lude.Text
dtacLanguage = Lens.lens (language :: DescribeTrustedAdvisorChecks -> Lude.Text) (\s a -> s {language = a} :: DescribeTrustedAdvisorChecks)
{-# DEPRECATED dtacLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Lude.AWSRequest DescribeTrustedAdvisorChecks where
  type
    Rs DescribeTrustedAdvisorChecks =
      DescribeTrustedAdvisorChecksResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorChecksResponse'
            Lude.<$> (x Lude..?> "checks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrustedAdvisorChecks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorChecks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrustedAdvisorChecks where
  toJSON DescribeTrustedAdvisorChecks' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("language" Lude..= language)])

instance Lude.ToPath DescribeTrustedAdvisorChecks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrustedAdvisorChecks where
  toQuery = Lude.const Lude.mempty

-- | Information about the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorChecks' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorChecksResponse' smart constructor.
data DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse'
  { -- | Information about all available Trusted Advisor checks.
    checks :: [TrustedAdvisorCheckDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorChecksResponse' with the minimum fields required to make a request.
--
-- * 'checks' - Information about all available Trusted Advisor checks.
-- * 'responseStatus' - The response status code.
mkDescribeTrustedAdvisorChecksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrustedAdvisorChecksResponse
mkDescribeTrustedAdvisorChecksResponse pResponseStatus_ =
  DescribeTrustedAdvisorChecksResponse'
    { checks = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Information about all available Trusted Advisor checks.
--
-- /Note:/ Consider using 'checks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrsChecks :: Lens.Lens' DescribeTrustedAdvisorChecksResponse [TrustedAdvisorCheckDescription]
dtacrsChecks = Lens.lens (checks :: DescribeTrustedAdvisorChecksResponse -> [TrustedAdvisorCheckDescription]) (\s a -> s {checks = a} :: DescribeTrustedAdvisorChecksResponse)
{-# DEPRECATED dtacrsChecks "Use generic-lens or generic-optics with 'checks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorChecksResponse Lude.Int
dtacrsResponseStatus = Lens.lens (responseStatus :: DescribeTrustedAdvisorChecksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrustedAdvisorChecksResponse)
{-# DEPRECATED dtacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
