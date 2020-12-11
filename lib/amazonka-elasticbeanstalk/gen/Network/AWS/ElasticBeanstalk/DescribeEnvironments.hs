{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing environments.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
  ( -- * Creating a request
    DescribeEnvironments (..),
    mkDescribeEnvironments,

    -- ** Request lenses
    desEnvironmentIds,
    desEnvironmentNames,
    desNextToken,
    desVersionLabel,
    desMaxRecords,
    desApplicationName,
    desIncludedDeletedBackTo,
    desIncludeDeleted,

    -- * Destructuring the response
    EnvironmentDescriptionsMessage (..),
    mkEnvironmentDescriptionsMessage,

    -- ** Response lenses
    edmNextToken,
    edmEnvironments,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe one or more environments.
--
-- /See:/ 'mkDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { environmentIds ::
      Lude.Maybe [Lude.Text],
    environmentNames :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    versionLabel :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Natural,
    applicationName :: Lude.Maybe Lude.Text,
    includedDeletedBackTo :: Lude.Maybe Lude.ISO8601,
    includeDeleted :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironments' with the minimum fields required to make a request.
--
-- * 'applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
-- * 'environmentIds' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
-- * 'environmentNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
-- * 'includeDeleted' - Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
-- * 'includedDeletedBackTo' - If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
-- * 'maxRecords' - For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
-- * 'nextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
-- * 'versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
mkDescribeEnvironments ::
  DescribeEnvironments
mkDescribeEnvironments =
  DescribeEnvironments'
    { environmentIds = Lude.Nothing,
      environmentNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      versionLabel = Lude.Nothing,
      maxRecords = Lude.Nothing,
      applicationName = Lude.Nothing,
      includedDeletedBackTo = Lude.Nothing,
      includeDeleted = Lude.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentIds :: Lens.Lens' DescribeEnvironments (Lude.Maybe [Lude.Text])
desEnvironmentIds = Lens.lens (environmentIds :: DescribeEnvironments -> Lude.Maybe [Lude.Text]) (\s a -> s {environmentIds = a} :: DescribeEnvironments)
{-# DEPRECATED desEnvironmentIds "Use generic-lens or generic-optics with 'environmentIds' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
--
-- /Note:/ Consider using 'environmentNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentNames :: Lens.Lens' DescribeEnvironments (Lude.Maybe [Lude.Text])
desEnvironmentNames = Lens.lens (environmentNames :: DescribeEnvironments -> Lude.Maybe [Lude.Text]) (\s a -> s {environmentNames = a} :: DescribeEnvironments)
{-# DEPRECATED desEnvironmentNames "Use generic-lens or generic-optics with 'environmentNames' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desNextToken :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Text)
desNextToken = Lens.lens (nextToken :: DescribeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEnvironments)
{-# DEPRECATED desNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desVersionLabel :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Text)
desVersionLabel = Lens.lens (versionLabel :: DescribeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: DescribeEnvironments)
{-# DEPRECATED desVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desMaxRecords :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Natural)
desMaxRecords = Lens.lens (maxRecords :: DescribeEnvironments -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeEnvironments)
{-# DEPRECATED desMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desApplicationName :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Text)
desApplicationName = Lens.lens (applicationName :: DescribeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DescribeEnvironments)
{-# DEPRECATED desApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
--
-- /Note:/ Consider using 'includedDeletedBackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIncludedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.ISO8601)
desIncludedDeletedBackTo = Lens.lens (includedDeletedBackTo :: DescribeEnvironments -> Lude.Maybe Lude.ISO8601) (\s a -> s {includedDeletedBackTo = a} :: DescribeEnvironments)
{-# DEPRECATED desIncludedDeletedBackTo "Use generic-lens or generic-optics with 'includedDeletedBackTo' instead." #-}

-- | Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
--
-- /Note:/ Consider using 'includeDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIncludeDeleted :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Bool)
desIncludeDeleted = Lens.lens (includeDeleted :: DescribeEnvironments -> Lude.Maybe Lude.Bool) (\s a -> s {includeDeleted = a} :: DescribeEnvironments)
{-# DEPRECATED desIncludeDeleted "Use generic-lens or generic-optics with 'includeDeleted' instead." #-}

instance Page.AWSPager DescribeEnvironments where
  page rq rs
    | Page.stop (rs Lens.^. edmNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. edmEnvironments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& desNextToken Lens..~ rs Lens.^. edmNextToken

instance Lude.AWSRequest DescribeEnvironments where
  type Rs DescribeEnvironments = EnvironmentDescriptionsMessage
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeEnvironmentsResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DescribeEnvironments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEnvironments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironments where
  toQuery DescribeEnvironments' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeEnvironments" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> environmentIds),
        "EnvironmentNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> environmentNames),
        "NextToken" Lude.=: nextToken,
        "VersionLabel" Lude.=: versionLabel,
        "MaxRecords" Lude.=: maxRecords,
        "ApplicationName" Lude.=: applicationName,
        "IncludedDeletedBackTo" Lude.=: includedDeletedBackTo,
        "IncludeDeleted" Lude.=: includeDeleted
      ]
