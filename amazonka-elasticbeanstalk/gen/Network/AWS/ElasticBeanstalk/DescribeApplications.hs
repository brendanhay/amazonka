{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of existing applications.
module Network.AWS.ElasticBeanstalk.DescribeApplications
  ( -- * Creating a Request
    DescribeApplications (..),
    newDescribeApplications,

    -- * Request Lenses
    describeApplications_applicationNames,

    -- * Destructuring the Response
    DescribeApplicationsResponse (..),
    newDescribeApplicationsResponse,

    -- * Response Lenses
    describeApplicationsResponse_applications,
    describeApplicationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe one or more applications.
--
-- /See:/ 'newDescribeApplications' smart constructor.
data DescribeApplications = DescribeApplications'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to only include those with the specified names.
    applicationNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationNames', 'describeApplications_applicationNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include those with the specified names.
newDescribeApplications ::
  DescribeApplications
newDescribeApplications =
  DescribeApplications'
    { applicationNames =
        Core.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include those with the specified names.
describeApplications_applicationNames :: Lens.Lens' DescribeApplications (Core.Maybe [Core.Text])
describeApplications_applicationNames = Lens.lens (\DescribeApplications' {applicationNames} -> applicationNames) (\s@DescribeApplications' {} a -> s {applicationNames = a} :: DescribeApplications) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeApplications where
  type
    AWSResponse DescribeApplications =
      DescribeApplicationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeApplicationsResult"
      ( \s h x ->
          DescribeApplicationsResponse'
            Core.<$> ( x Core..@? "Applications" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeApplications

instance Core.NFData DescribeApplications

instance Core.ToHeaders DescribeApplications where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeApplications where
  toPath = Core.const "/"

instance Core.ToQuery DescribeApplications where
  toQuery DescribeApplications' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeApplications" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ApplicationNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> applicationNames
            )
      ]

-- | Result message containing a list of application descriptions.
--
-- /See:/ 'newDescribeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { -- | This parameter contains a list of ApplicationDescription.
    applications :: Core.Maybe [ApplicationDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applications', 'describeApplicationsResponse_applications' - This parameter contains a list of ApplicationDescription.
--
-- 'httpStatus', 'describeApplicationsResponse_httpStatus' - The response's http status code.
newDescribeApplicationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeApplicationsResponse
newDescribeApplicationsResponse pHttpStatus_ =
  DescribeApplicationsResponse'
    { applications =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This parameter contains a list of ApplicationDescription.
describeApplicationsResponse_applications :: Lens.Lens' DescribeApplicationsResponse (Core.Maybe [ApplicationDescription])
describeApplicationsResponse_applications = Lens.lens (\DescribeApplicationsResponse' {applications} -> applications) (\s@DescribeApplicationsResponse' {} a -> s {applications = a} :: DescribeApplicationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeApplicationsResponse_httpStatus :: Lens.Lens' DescribeApplicationsResponse Core.Int
describeApplicationsResponse_httpStatus = Lens.lens (\DescribeApplicationsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationsResponse)

instance Core.NFData DescribeApplicationsResponse
