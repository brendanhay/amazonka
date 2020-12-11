{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about IAM entity usage and IAM quotas in the AWS account.
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.GetAccountSummary
  ( -- * Creating a request
    GetAccountSummary (..),
    mkGetAccountSummary,

    -- * Destructuring the response
    GetAccountSummaryResponse (..),
    mkGetAccountSummaryResponse,

    -- ** Response lenses
    gasrsSummaryMap,
    gasrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAccountSummary' smart constructor.
data GetAccountSummary = GetAccountSummary'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountSummary' with the minimum fields required to make a request.
mkGetAccountSummary ::
  GetAccountSummary
mkGetAccountSummary = GetAccountSummary'

instance Lude.AWSRequest GetAccountSummary where
  type Rs GetAccountSummary = GetAccountSummaryResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetAccountSummaryResult"
      ( \s h x ->
          GetAccountSummaryResponse'
            Lude.<$> ( x Lude..@? "SummaryMap" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountSummary where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccountSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccountSummary where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetAccountSummary" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'GetAccountSummary' request.
--
-- /See:/ 'mkGetAccountSummaryResponse' smart constructor.
data GetAccountSummaryResponse = GetAccountSummaryResponse'
  { summaryMap ::
      Lude.Maybe
        ( Lude.HashMap
            SummaryKeyType
            (Lude.Int)
        ),
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

-- | Creates a value of 'GetAccountSummaryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'summaryMap' - A set of key–value pairs containing information about IAM entity usage and IAM quotas.
mkGetAccountSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountSummaryResponse
mkGetAccountSummaryResponse pResponseStatus_ =
  GetAccountSummaryResponse'
    { summaryMap = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of key–value pairs containing information about IAM entity usage and IAM quotas.
--
-- /Note:/ Consider using 'summaryMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsSummaryMap :: Lens.Lens' GetAccountSummaryResponse (Lude.Maybe (Lude.HashMap SummaryKeyType (Lude.Int)))
gasrsSummaryMap = Lens.lens (summaryMap :: GetAccountSummaryResponse -> Lude.Maybe (Lude.HashMap SummaryKeyType (Lude.Int))) (\s a -> s {summaryMap = a} :: GetAccountSummaryResponse)
{-# DEPRECATED gasrsSummaryMap "Use generic-lens or generic-optics with 'summaryMap' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResponseStatus :: Lens.Lens' GetAccountSummaryResponse Lude.Int
gasrsResponseStatus = Lens.lens (responseStatus :: GetAccountSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountSummaryResponse)
{-# DEPRECATED gasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
