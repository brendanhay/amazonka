{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListCustomVerificationEmailTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing custom verification email templates for your account in the current AWS Region.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListCustomVerificationEmailTemplates
  ( -- * Creating a request
    ListCustomVerificationEmailTemplates (..),
    mkListCustomVerificationEmailTemplates,

    -- ** Request lenses
    lcvetNextToken,
    lcvetMaxResults,

    -- * Destructuring the response
    ListCustomVerificationEmailTemplatesResponse (..),
    mkListCustomVerificationEmailTemplatesResponse,

    -- ** Response lenses
    lcvetrsNextToken,
    lcvetrsCustomVerificationEmailTemplates,
    lcvetrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to list the existing custom verification email templates for your account.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- /See:/ 'mkListCustomVerificationEmailTemplates' smart constructor.
data ListCustomVerificationEmailTemplates = ListCustomVerificationEmailTemplates'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCustomVerificationEmailTemplates' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of custom verification email templates to return. This value must be at least 1 and less than or equal to 50. If you do not specify a value, or if you specify a value less than 1 or greater than 50, the operation will return up to 50 results.
-- * 'nextToken' - An array the contains the name and creation time stamp for each template in your Amazon SES account.
mkListCustomVerificationEmailTemplates ::
  ListCustomVerificationEmailTemplates
mkListCustomVerificationEmailTemplates =
  ListCustomVerificationEmailTemplates'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetNextToken :: Lens.Lens' ListCustomVerificationEmailTemplates (Lude.Maybe Lude.Text)
lcvetNextToken = Lens.lens (nextToken :: ListCustomVerificationEmailTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCustomVerificationEmailTemplates)
{-# DEPRECATED lcvetNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of custom verification email templates to return. This value must be at least 1 and less than or equal to 50. If you do not specify a value, or if you specify a value less than 1 or greater than 50, the operation will return up to 50 results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetMaxResults :: Lens.Lens' ListCustomVerificationEmailTemplates (Lude.Maybe Lude.Natural)
lcvetMaxResults = Lens.lens (maxResults :: ListCustomVerificationEmailTemplates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCustomVerificationEmailTemplates)
{-# DEPRECATED lcvetMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCustomVerificationEmailTemplates where
  page rq rs
    | Page.stop (rs Lens.^. lcvetrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcvetrsCustomVerificationEmailTemplates) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcvetNextToken Lens..~ rs Lens.^. lcvetrsNextToken

instance Lude.AWSRequest ListCustomVerificationEmailTemplates where
  type
    Rs ListCustomVerificationEmailTemplates =
      ListCustomVerificationEmailTemplatesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListCustomVerificationEmailTemplatesResult"
      ( \s h x ->
          ListCustomVerificationEmailTemplatesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "CustomVerificationEmailTemplates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCustomVerificationEmailTemplates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCustomVerificationEmailTemplates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCustomVerificationEmailTemplates where
  toQuery ListCustomVerificationEmailTemplates' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListCustomVerificationEmailTemplates" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | A paginated list of custom verification email templates.
--
-- /See:/ 'mkListCustomVerificationEmailTemplatesResponse' smart constructor.
data ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    customVerificationEmailTemplates ::
      Lude.Maybe
        [CustomVerificationEmailTemplate],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCustomVerificationEmailTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'customVerificationEmailTemplates' - A list of the custom verification email templates that exist in your account.
-- * 'nextToken' - A token indicating that there are additional custom verification email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 custom verification email templates.
-- * 'responseStatus' - The response status code.
mkListCustomVerificationEmailTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCustomVerificationEmailTemplatesResponse
mkListCustomVerificationEmailTemplatesResponse pResponseStatus_ =
  ListCustomVerificationEmailTemplatesResponse'
    { nextToken =
        Lude.Nothing,
      customVerificationEmailTemplates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token indicating that there are additional custom verification email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 custom verification email templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetrsNextToken :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Lude.Maybe Lude.Text)
lcvetrsNextToken = Lens.lens (nextToken :: ListCustomVerificationEmailTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCustomVerificationEmailTemplatesResponse)
{-# DEPRECATED lcvetrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the custom verification email templates that exist in your account.
--
-- /Note:/ Consider using 'customVerificationEmailTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetrsCustomVerificationEmailTemplates :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Lude.Maybe [CustomVerificationEmailTemplate])
lcvetrsCustomVerificationEmailTemplates = Lens.lens (customVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> Lude.Maybe [CustomVerificationEmailTemplate]) (\s a -> s {customVerificationEmailTemplates = a} :: ListCustomVerificationEmailTemplatesResponse)
{-# DEPRECATED lcvetrsCustomVerificationEmailTemplates "Use generic-lens or generic-optics with 'customVerificationEmailTemplates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcvetrsResponseStatus :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse Lude.Int
lcvetrsResponseStatus = Lens.lens (responseStatus :: ListCustomVerificationEmailTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCustomVerificationEmailTemplatesResponse)
{-# DEPRECATED lcvetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
