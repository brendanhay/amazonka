{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    deEnvironmentIds,
    deEnvironmentNames,
    deNextToken,
    deVersionLabel,
    deMaxRecords,
    deApplicationName,
    deIncludedDeletedBackTo,
    deIncludeDeleted,

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
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
    environmentIds :: Lude.Maybe [Lude.Text],
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
    environmentNames :: Lude.Maybe [Lude.Text],
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Lude.Maybe Lude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
    versionLabel :: Lude.Maybe Lude.Text,
    -- | For a paginated request. Specify a maximum number of environments to include in each response.
    --
    -- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
    maxRecords :: Lude.Maybe Lude.Natural,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
    applicationName :: Lude.Maybe Lude.Text,
    -- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
    includedDeletedBackTo :: Lude.Maybe Lude.DateTime,
    -- | Indicates whether to include deleted environments:
    --
    -- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
    -- @false@ : Do not include deleted environments.
    includeDeleted :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironments' with the minimum fields required to make a request.
--
-- * 'environmentIds' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
-- * 'environmentNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
-- * 'nextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
-- * 'versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
-- * 'maxRecords' - For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
-- * 'applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
-- * 'includedDeletedBackTo' - If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
-- * 'includeDeleted' - Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
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
deEnvironmentIds :: Lens.Lens' DescribeEnvironments (Lude.Maybe [Lude.Text])
deEnvironmentIds = Lens.lens (environmentIds :: DescribeEnvironments -> Lude.Maybe [Lude.Text]) (\s a -> s {environmentIds = a} :: DescribeEnvironments)
{-# DEPRECATED deEnvironmentIds "Use generic-lens or generic-optics with 'environmentIds' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
--
-- /Note:/ Consider using 'environmentNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentNames :: Lens.Lens' DescribeEnvironments (Lude.Maybe [Lude.Text])
deEnvironmentNames = Lens.lens (environmentNames :: DescribeEnvironments -> Lude.Maybe [Lude.Text]) (\s a -> s {environmentNames = a} :: DescribeEnvironments)
{-# DEPRECATED deEnvironmentNames "Use generic-lens or generic-optics with 'environmentNames' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEnvironments)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deVersionLabel :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Text)
deVersionLabel = Lens.lens (versionLabel :: DescribeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: DescribeEnvironments)
{-# DEPRECATED deVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Natural)
deMaxRecords = Lens.lens (maxRecords :: DescribeEnvironments -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeEnvironments)
{-# DEPRECATED deMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deApplicationName :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Text)
deApplicationName = Lens.lens (applicationName :: DescribeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DescribeEnvironments)
{-# DEPRECATED deApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
--
-- /Note:/ Consider using 'includedDeletedBackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deIncludedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.DateTime)
deIncludedDeletedBackTo = Lens.lens (includedDeletedBackTo :: DescribeEnvironments -> Lude.Maybe Lude.DateTime) (\s a -> s {includedDeletedBackTo = a} :: DescribeEnvironments)
{-# DEPRECATED deIncludedDeletedBackTo "Use generic-lens or generic-optics with 'includedDeletedBackTo' instead." #-}

-- | Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
--
-- /Note:/ Consider using 'includeDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deIncludeDeleted :: Lens.Lens' DescribeEnvironments (Lude.Maybe Lude.Bool)
deIncludeDeleted = Lens.lens (includeDeleted :: DescribeEnvironments -> Lude.Maybe Lude.Bool) (\s a -> s {includeDeleted = a} :: DescribeEnvironments)
{-# DEPRECATED deIncludeDeleted "Use generic-lens or generic-optics with 'includeDeleted' instead." #-}

instance Page.AWSPager DescribeEnvironments where
  page rq rs
    | Page.stop (rs Lens.^. edmNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. edmEnvironments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deNextToken Lens..~ rs Lens.^. edmNextToken

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
