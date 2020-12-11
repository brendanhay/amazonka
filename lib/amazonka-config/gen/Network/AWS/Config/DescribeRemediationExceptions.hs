{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation exceptions. A detailed view of a remediation exception for a set of resources that includes an explanation of an exception and the time when the exception will be deleted. When you specify the limit and the next token, you receive a paginated response.
module Network.AWS.Config.DescribeRemediationExceptions
  ( -- * Creating a request
    DescribeRemediationExceptions (..),
    mkDescribeRemediationExceptions,

    -- ** Request lenses
    dreNextToken,
    dreLimit,
    dreResourceKeys,
    dreConfigRuleName,

    -- * Destructuring the response
    DescribeRemediationExceptionsResponse (..),
    mkDescribeRemediationExceptionsResponse,

    -- ** Response lenses
    drersNextToken,
    drersRemediationExceptions,
    drersResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRemediationExceptions' smart constructor.
data DescribeRemediationExceptions = DescribeRemediationExceptions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Natural,
    resourceKeys ::
      Lude.Maybe
        ( Lude.NonEmpty
            RemediationExceptionResourceKey
        ),
    configRuleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRemediationExceptions' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule.
-- * 'limit' - The maximum number of RemediationExceptionResourceKey returned on each page. The default is 25. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'resourceKeys' - An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
mkDescribeRemediationExceptions ::
  -- | 'configRuleName'
  Lude.Text ->
  DescribeRemediationExceptions
mkDescribeRemediationExceptions pConfigRuleName_ =
  DescribeRemediationExceptions'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      resourceKeys = Lude.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreNextToken :: Lens.Lens' DescribeRemediationExceptions (Lude.Maybe Lude.Text)
dreNextToken = Lens.lens (nextToken :: DescribeRemediationExceptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRemediationExceptions)
{-# DEPRECATED dreNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of RemediationExceptionResourceKey returned on each page. The default is 25. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreLimit :: Lens.Lens' DescribeRemediationExceptions (Lude.Maybe Lude.Natural)
dreLimit = Lens.lens (limit :: DescribeRemediationExceptions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeRemediationExceptions)
{-# DEPRECATED dreLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreResourceKeys :: Lens.Lens' DescribeRemediationExceptions (Lude.Maybe (Lude.NonEmpty RemediationExceptionResourceKey))
dreResourceKeys = Lens.lens (resourceKeys :: DescribeRemediationExceptions -> Lude.Maybe (Lude.NonEmpty RemediationExceptionResourceKey)) (\s a -> s {resourceKeys = a} :: DescribeRemediationExceptions)
{-# DEPRECATED dreResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreConfigRuleName :: Lens.Lens' DescribeRemediationExceptions Lude.Text
dreConfigRuleName = Lens.lens (configRuleName :: DescribeRemediationExceptions -> Lude.Text) (\s a -> s {configRuleName = a} :: DescribeRemediationExceptions)
{-# DEPRECATED dreConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Lude.AWSRequest DescribeRemediationExceptions where
  type
    Rs DescribeRemediationExceptions =
      DescribeRemediationExceptionsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRemediationExceptionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "RemediationExceptions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRemediationExceptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeRemediationExceptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRemediationExceptions where
  toJSON DescribeRemediationExceptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("ResourceKeys" Lude..=) Lude.<$> resourceKeys,
            Lude.Just ("ConfigRuleName" Lude..= configRuleName)
          ]
      )

instance Lude.ToPath DescribeRemediationExceptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRemediationExceptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRemediationExceptionsResponse' smart constructor.
data DescribeRemediationExceptionsResponse = DescribeRemediationExceptionsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    remediationExceptions ::
      Lude.Maybe
        [RemediationException],
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

-- | Creates a value of 'DescribeRemediationExceptionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'remediationExceptions' - Returns a list of remediation exception objects.
-- * 'responseStatus' - The response status code.
mkDescribeRemediationExceptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRemediationExceptionsResponse
mkDescribeRemediationExceptionsResponse pResponseStatus_ =
  DescribeRemediationExceptionsResponse'
    { nextToken = Lude.Nothing,
      remediationExceptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drersNextToken :: Lens.Lens' DescribeRemediationExceptionsResponse (Lude.Maybe Lude.Text)
drersNextToken = Lens.lens (nextToken :: DescribeRemediationExceptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRemediationExceptionsResponse)
{-# DEPRECATED drersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of remediation exception objects.
--
-- /Note:/ Consider using 'remediationExceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drersRemediationExceptions :: Lens.Lens' DescribeRemediationExceptionsResponse (Lude.Maybe [RemediationException])
drersRemediationExceptions = Lens.lens (remediationExceptions :: DescribeRemediationExceptionsResponse -> Lude.Maybe [RemediationException]) (\s a -> s {remediationExceptions = a} :: DescribeRemediationExceptionsResponse)
{-# DEPRECATED drersRemediationExceptions "Use generic-lens or generic-optics with 'remediationExceptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drersResponseStatus :: Lens.Lens' DescribeRemediationExceptionsResponse Lude.Int
drersResponseStatus = Lens.lens (responseStatus :: DescribeRemediationExceptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRemediationExceptionsResponse)
{-# DEPRECATED drersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
