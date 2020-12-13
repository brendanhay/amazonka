{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of existing applications.
module Network.AWS.ElasticBeanstalk.DescribeApplications
  ( -- * Creating a request
    DescribeApplications (..),
    mkDescribeApplications,

    -- ** Request lenses
    daApplicationNames,

    -- * Destructuring the response
    DescribeApplicationsResponse (..),
    mkDescribeApplicationsResponse,

    -- ** Response lenses
    darsApplications,
    darsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe one or more applications.
--
-- /See:/ 'mkDescribeApplications' smart constructor.
newtype DescribeApplications = DescribeApplications'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
    applicationNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplications' with the minimum fields required to make a request.
--
-- * 'applicationNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
mkDescribeApplications ::
  DescribeApplications
mkDescribeApplications =
  DescribeApplications' {applicationNames = Lude.Nothing}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
--
-- /Note:/ Consider using 'applicationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationNames :: Lens.Lens' DescribeApplications (Lude.Maybe [Lude.Text])
daApplicationNames = Lens.lens (applicationNames :: DescribeApplications -> Lude.Maybe [Lude.Text]) (\s a -> s {applicationNames = a} :: DescribeApplications)
{-# DEPRECATED daApplicationNames "Use generic-lens or generic-optics with 'applicationNames' instead." #-}

instance Lude.AWSRequest DescribeApplications where
  type Rs DescribeApplications = DescribeApplicationsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeApplicationsResult"
      ( \s h x ->
          DescribeApplicationsResponse'
            Lude.<$> ( x Lude..@? "Applications" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeApplications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeApplications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApplications where
  toQuery DescribeApplications' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeApplications" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ApplicationNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> applicationNames)
      ]

-- | Result message containing a list of application descriptions.
--
-- /See:/ 'mkDescribeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { -- | This parameter contains a list of 'ApplicationDescription' .
    applications :: Lude.Maybe [ApplicationDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'applications' - This parameter contains a list of 'ApplicationDescription' .
-- * 'responseStatus' - The response status code.
mkDescribeApplicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeApplicationsResponse
mkDescribeApplicationsResponse pResponseStatus_ =
  DescribeApplicationsResponse'
    { applications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This parameter contains a list of 'ApplicationDescription' .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsApplications :: Lens.Lens' DescribeApplicationsResponse (Lude.Maybe [ApplicationDescription])
darsApplications = Lens.lens (applications :: DescribeApplicationsResponse -> Lude.Maybe [ApplicationDescription]) (\s a -> s {applications = a} :: DescribeApplicationsResponse)
{-# DEPRECATED darsApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeApplicationsResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeApplicationsResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
