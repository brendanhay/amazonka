{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the findings that are specified by the ARNs of the findings.
module Network.AWS.Inspector.DescribeFindings
  ( -- * Creating a request
    DescribeFindings (..),
    mkDescribeFindings,

    -- ** Request lenses
    dfLocale,
    dfFindingARNs,

    -- * Destructuring the response
    DescribeFindingsResponse (..),
    mkDescribeFindingsResponse,

    -- ** Response lenses
    dfrsFailedItems,
    dfrsFindings,
    dfrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFindings' smart constructor.
data DescribeFindings = DescribeFindings'
  { -- | The locale into which you want to translate a finding description, recommendation, and the short description that identifies the finding.
    locale :: Lude.Maybe Locale,
    -- | The ARN that specifies the finding that you want to describe.
    findingARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFindings' with the minimum fields required to make a request.
--
-- * 'locale' - The locale into which you want to translate a finding description, recommendation, and the short description that identifies the finding.
-- * 'findingARNs' - The ARN that specifies the finding that you want to describe.
mkDescribeFindings ::
  -- | 'findingARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeFindings
mkDescribeFindings pFindingARNs_ =
  DescribeFindings'
    { locale = Lude.Nothing,
      findingARNs = pFindingARNs_
    }

-- | The locale into which you want to translate a finding description, recommendation, and the short description that identifies the finding.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLocale :: Lens.Lens' DescribeFindings (Lude.Maybe Locale)
dfLocale = Lens.lens (locale :: DescribeFindings -> Lude.Maybe Locale) (\s a -> s {locale = a} :: DescribeFindings)
{-# DEPRECATED dfLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The ARN that specifies the finding that you want to describe.
--
-- /Note:/ Consider using 'findingARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFindingARNs :: Lens.Lens' DescribeFindings (Lude.NonEmpty Lude.Text)
dfFindingARNs = Lens.lens (findingARNs :: DescribeFindings -> Lude.NonEmpty Lude.Text) (\s a -> s {findingARNs = a} :: DescribeFindings)
{-# DEPRECATED dfFindingARNs "Use generic-lens or generic-optics with 'findingARNs' instead." #-}

instance Lude.AWSRequest DescribeFindings where
  type Rs DescribeFindings = DescribeFindingsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFindingsResponse'
            Lude.<$> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "findings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DescribeFindings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFindings where
  toJSON DescribeFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            Lude.Just ("findingArns" Lude..= findingARNs)
          ]
      )

instance Lude.ToPath DescribeFindings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeFindingsResponse' smart constructor.
data DescribeFindingsResponse = DescribeFindingsResponse'
  { -- | Finding details that cannot be described. An error code is provided for each failed item.
    failedItems :: Lude.HashMap Lude.Text (FailedItemDetails),
    -- | Information about the finding.
    findings :: [Finding],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFindingsResponse' with the minimum fields required to make a request.
--
-- * 'failedItems' - Finding details that cannot be described. An error code is provided for each failed item.
-- * 'findings' - Information about the finding.
-- * 'responseStatus' - The response status code.
mkDescribeFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFindingsResponse
mkDescribeFindingsResponse pResponseStatus_ =
  DescribeFindingsResponse'
    { failedItems = Lude.mempty,
      findings = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Finding details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsFailedItems :: Lens.Lens' DescribeFindingsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
dfrsFailedItems = Lens.lens (failedItems :: DescribeFindingsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeFindingsResponse)
{-# DEPRECATED dfrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | Information about the finding.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsFindings :: Lens.Lens' DescribeFindingsResponse [Finding]
dfrsFindings = Lens.lens (findings :: DescribeFindingsResponse -> [Finding]) (\s a -> s {findings = a} :: DescribeFindingsResponse)
{-# DEPRECATED dfrsFindings "Use generic-lens or generic-optics with 'findings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DescribeFindingsResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DescribeFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFindingsResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
