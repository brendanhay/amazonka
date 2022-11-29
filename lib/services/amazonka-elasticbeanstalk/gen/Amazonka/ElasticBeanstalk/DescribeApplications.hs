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
-- Module      : Amazonka.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of existing applications.
module Amazonka.ElasticBeanstalk.DescribeApplications
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe one or more applications.
--
-- /See:/ 'newDescribeApplications' smart constructor.
data DescribeApplications = DescribeApplications'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to only include those with the specified names.
    applicationNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include those with the specified names.
describeApplications_applicationNames :: Lens.Lens' DescribeApplications (Prelude.Maybe [Prelude.Text])
describeApplications_applicationNames = Lens.lens (\DescribeApplications' {applicationNames} -> applicationNames) (\s@DescribeApplications' {} a -> s {applicationNames = a} :: DescribeApplications) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeApplications where
  type
    AWSResponse DescribeApplications =
      DescribeApplicationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeApplicationsResult"
      ( \s h x ->
          DescribeApplicationsResponse'
            Prelude.<$> ( x Core..@? "Applications" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApplications where
  hashWithSalt _salt DescribeApplications' {..} =
    _salt `Prelude.hashWithSalt` applicationNames

instance Prelude.NFData DescribeApplications where
  rnf DescribeApplications' {..} =
    Prelude.rnf applicationNames

instance Core.ToHeaders DescribeApplications where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeApplications where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeApplications where
  toQuery DescribeApplications' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeApplications" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> applicationNames
            )
      ]

-- | Result message containing a list of application descriptions.
--
-- /See:/ 'newDescribeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { -- | This parameter contains a list of ApplicationDescription.
    applications :: Prelude.Maybe [ApplicationDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeApplicationsResponse
newDescribeApplicationsResponse pHttpStatus_ =
  DescribeApplicationsResponse'
    { applications =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This parameter contains a list of ApplicationDescription.
describeApplicationsResponse_applications :: Lens.Lens' DescribeApplicationsResponse (Prelude.Maybe [ApplicationDescription])
describeApplicationsResponse_applications = Lens.lens (\DescribeApplicationsResponse' {applications} -> applications) (\s@DescribeApplicationsResponse' {} a -> s {applications = a} :: DescribeApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeApplicationsResponse_httpStatus :: Lens.Lens' DescribeApplicationsResponse Prelude.Int
describeApplicationsResponse_httpStatus = Lens.lens (\DescribeApplicationsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationsResponse)

instance Prelude.NFData DescribeApplicationsResponse where
  rnf DescribeApplicationsResponse' {..} =
    Prelude.rnf applications
      `Prelude.seq` Prelude.rnf httpStatus
