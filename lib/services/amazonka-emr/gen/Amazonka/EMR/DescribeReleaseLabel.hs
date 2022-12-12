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
-- Module      : Amazonka.EMR.DescribeReleaseLabel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides EMR release label details, such as releases available the
-- region where the API request is run, and the available applications for
-- a specific EMR release label. Can also list EMR release versions that
-- support a specified version of Spark.
module Amazonka.EMR.DescribeReleaseLabel
  ( -- * Creating a Request
    DescribeReleaseLabel (..),
    newDescribeReleaseLabel,

    -- * Request Lenses
    describeReleaseLabel_maxResults,
    describeReleaseLabel_nextToken,
    describeReleaseLabel_releaseLabel,

    -- * Destructuring the Response
    DescribeReleaseLabelResponse (..),
    newDescribeReleaseLabelResponse,

    -- * Response Lenses
    describeReleaseLabelResponse_applications,
    describeReleaseLabelResponse_availableOSReleases,
    describeReleaseLabelResponse_nextToken,
    describeReleaseLabelResponse_releaseLabel,
    describeReleaseLabelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReleaseLabel' smart constructor.
data DescribeReleaseLabel = DescribeReleaseLabel'
  { -- | Reserved for future use. Currently set to null.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token. Reserved for future use. Currently set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The target release label to be described.
    releaseLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReleaseLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeReleaseLabel_maxResults' - Reserved for future use. Currently set to null.
--
-- 'nextToken', 'describeReleaseLabel_nextToken' - The pagination token. Reserved for future use. Currently set to null.
--
-- 'releaseLabel', 'describeReleaseLabel_releaseLabel' - The target release label to be described.
newDescribeReleaseLabel ::
  DescribeReleaseLabel
newDescribeReleaseLabel =
  DescribeReleaseLabel'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      releaseLabel = Prelude.Nothing
    }

-- | Reserved for future use. Currently set to null.
describeReleaseLabel_maxResults :: Lens.Lens' DescribeReleaseLabel (Prelude.Maybe Prelude.Natural)
describeReleaseLabel_maxResults = Lens.lens (\DescribeReleaseLabel' {maxResults} -> maxResults) (\s@DescribeReleaseLabel' {} a -> s {maxResults = a} :: DescribeReleaseLabel)

-- | The pagination token. Reserved for future use. Currently set to null.
describeReleaseLabel_nextToken :: Lens.Lens' DescribeReleaseLabel (Prelude.Maybe Prelude.Text)
describeReleaseLabel_nextToken = Lens.lens (\DescribeReleaseLabel' {nextToken} -> nextToken) (\s@DescribeReleaseLabel' {} a -> s {nextToken = a} :: DescribeReleaseLabel)

-- | The target release label to be described.
describeReleaseLabel_releaseLabel :: Lens.Lens' DescribeReleaseLabel (Prelude.Maybe Prelude.Text)
describeReleaseLabel_releaseLabel = Lens.lens (\DescribeReleaseLabel' {releaseLabel} -> releaseLabel) (\s@DescribeReleaseLabel' {} a -> s {releaseLabel = a} :: DescribeReleaseLabel)

instance Core.AWSRequest DescribeReleaseLabel where
  type
    AWSResponse DescribeReleaseLabel =
      DescribeReleaseLabelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReleaseLabelResponse'
            Prelude.<$> (x Data..?> "Applications" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "AvailableOSReleases"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ReleaseLabel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReleaseLabel where
  hashWithSalt _salt DescribeReleaseLabel' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` releaseLabel

instance Prelude.NFData DescribeReleaseLabel where
  rnf DescribeReleaseLabel' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf releaseLabel

instance Data.ToHeaders DescribeReleaseLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.DescribeReleaseLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReleaseLabel where
  toJSON DescribeReleaseLabel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ReleaseLabel" Data..=) Prelude.<$> releaseLabel
          ]
      )

instance Data.ToPath DescribeReleaseLabel where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReleaseLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReleaseLabelResponse' smart constructor.
data DescribeReleaseLabelResponse = DescribeReleaseLabelResponse'
  { -- | The list of applications available for the target release label. @Name@
    -- is the name of the application. @Version@ is the concise version of the
    -- application.
    applications :: Prelude.Maybe [SimplifiedApplication],
    -- | The list of available Amazon Linux release versions for an Amazon EMR
    -- release. Contains a Label field that is formatted as shown in
    -- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-al2.html Amazon Linux 2 Release Notes>
    -- . For example,
    -- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-20220218.html 2.0.20220218.1>.
    availableOSReleases :: Prelude.Maybe [OSRelease],
    -- | The pagination token. Reserved for future use. Currently set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The target release label described in the response.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReleaseLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applications', 'describeReleaseLabelResponse_applications' - The list of applications available for the target release label. @Name@
-- is the name of the application. @Version@ is the concise version of the
-- application.
--
-- 'availableOSReleases', 'describeReleaseLabelResponse_availableOSReleases' - The list of available Amazon Linux release versions for an Amazon EMR
-- release. Contains a Label field that is formatted as shown in
-- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-al2.html Amazon Linux 2 Release Notes>
-- . For example,
-- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-20220218.html 2.0.20220218.1>.
--
-- 'nextToken', 'describeReleaseLabelResponse_nextToken' - The pagination token. Reserved for future use. Currently set to null.
--
-- 'releaseLabel', 'describeReleaseLabelResponse_releaseLabel' - The target release label described in the response.
--
-- 'httpStatus', 'describeReleaseLabelResponse_httpStatus' - The response's http status code.
newDescribeReleaseLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReleaseLabelResponse
newDescribeReleaseLabelResponse pHttpStatus_ =
  DescribeReleaseLabelResponse'
    { applications =
        Prelude.Nothing,
      availableOSReleases = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of applications available for the target release label. @Name@
-- is the name of the application. @Version@ is the concise version of the
-- application.
describeReleaseLabelResponse_applications :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe [SimplifiedApplication])
describeReleaseLabelResponse_applications = Lens.lens (\DescribeReleaseLabelResponse' {applications} -> applications) (\s@DescribeReleaseLabelResponse' {} a -> s {applications = a} :: DescribeReleaseLabelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of available Amazon Linux release versions for an Amazon EMR
-- release. Contains a Label field that is formatted as shown in
-- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-al2.html Amazon Linux 2 Release Notes>
-- . For example,
-- <https://docs.aws.amazon.com/AL2/latest/relnotes/relnotes-20220218.html 2.0.20220218.1>.
describeReleaseLabelResponse_availableOSReleases :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe [OSRelease])
describeReleaseLabelResponse_availableOSReleases = Lens.lens (\DescribeReleaseLabelResponse' {availableOSReleases} -> availableOSReleases) (\s@DescribeReleaseLabelResponse' {} a -> s {availableOSReleases = a} :: DescribeReleaseLabelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token. Reserved for future use. Currently set to null.
describeReleaseLabelResponse_nextToken :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe Prelude.Text)
describeReleaseLabelResponse_nextToken = Lens.lens (\DescribeReleaseLabelResponse' {nextToken} -> nextToken) (\s@DescribeReleaseLabelResponse' {} a -> s {nextToken = a} :: DescribeReleaseLabelResponse)

-- | The target release label described in the response.
describeReleaseLabelResponse_releaseLabel :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe Prelude.Text)
describeReleaseLabelResponse_releaseLabel = Lens.lens (\DescribeReleaseLabelResponse' {releaseLabel} -> releaseLabel) (\s@DescribeReleaseLabelResponse' {} a -> s {releaseLabel = a} :: DescribeReleaseLabelResponse)

-- | The response's http status code.
describeReleaseLabelResponse_httpStatus :: Lens.Lens' DescribeReleaseLabelResponse Prelude.Int
describeReleaseLabelResponse_httpStatus = Lens.lens (\DescribeReleaseLabelResponse' {httpStatus} -> httpStatus) (\s@DescribeReleaseLabelResponse' {} a -> s {httpStatus = a} :: DescribeReleaseLabelResponse)

instance Prelude.NFData DescribeReleaseLabelResponse where
  rnf DescribeReleaseLabelResponse' {..} =
    Prelude.rnf applications
      `Prelude.seq` Prelude.rnf availableOSReleases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf httpStatus
