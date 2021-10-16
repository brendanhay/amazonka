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
-- Module      : Network.AWS.EMR.DescribeReleaseLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides EMR release label details, such as releases available the
-- region where the API request is run, and the available applications for
-- a specific EMR release label. Can also list EMR release versions that
-- support a specified version of Spark.
module Network.AWS.EMR.DescribeReleaseLabel
  ( -- * Creating a Request
    DescribeReleaseLabel (..),
    newDescribeReleaseLabel,

    -- * Request Lenses
    describeReleaseLabel_nextToken,
    describeReleaseLabel_maxResults,
    describeReleaseLabel_releaseLabel,

    -- * Destructuring the Response
    DescribeReleaseLabelResponse (..),
    newDescribeReleaseLabelResponse,

    -- * Response Lenses
    describeReleaseLabelResponse_nextToken,
    describeReleaseLabelResponse_releaseLabel,
    describeReleaseLabelResponse_applications,
    describeReleaseLabelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeReleaseLabel' smart constructor.
data DescribeReleaseLabel = DescribeReleaseLabel'
  { -- | The pagination token. Reserved for future use. Currently set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use. Currently set to null.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'describeReleaseLabel_nextToken' - The pagination token. Reserved for future use. Currently set to null.
--
-- 'maxResults', 'describeReleaseLabel_maxResults' - Reserved for future use. Currently set to null.
--
-- 'releaseLabel', 'describeReleaseLabel_releaseLabel' - The target release label to be described.
newDescribeReleaseLabel ::
  DescribeReleaseLabel
newDescribeReleaseLabel =
  DescribeReleaseLabel'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      releaseLabel = Prelude.Nothing
    }

-- | The pagination token. Reserved for future use. Currently set to null.
describeReleaseLabel_nextToken :: Lens.Lens' DescribeReleaseLabel (Prelude.Maybe Prelude.Text)
describeReleaseLabel_nextToken = Lens.lens (\DescribeReleaseLabel' {nextToken} -> nextToken) (\s@DescribeReleaseLabel' {} a -> s {nextToken = a} :: DescribeReleaseLabel)

-- | Reserved for future use. Currently set to null.
describeReleaseLabel_maxResults :: Lens.Lens' DescribeReleaseLabel (Prelude.Maybe Prelude.Natural)
describeReleaseLabel_maxResults = Lens.lens (\DescribeReleaseLabel' {maxResults} -> maxResults) (\s@DescribeReleaseLabel' {} a -> s {maxResults = a} :: DescribeReleaseLabel)

-- | The target release label to be described.
describeReleaseLabel_releaseLabel :: Lens.Lens' DescribeReleaseLabel (Prelude.Maybe Prelude.Text)
describeReleaseLabel_releaseLabel = Lens.lens (\DescribeReleaseLabel' {releaseLabel} -> releaseLabel) (\s@DescribeReleaseLabel' {} a -> s {releaseLabel = a} :: DescribeReleaseLabel)

instance Core.AWSRequest DescribeReleaseLabel where
  type
    AWSResponse DescribeReleaseLabel =
      DescribeReleaseLabelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReleaseLabelResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ReleaseLabel")
            Prelude.<*> (x Core..?> "Applications" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReleaseLabel

instance Prelude.NFData DescribeReleaseLabel

instance Core.ToHeaders DescribeReleaseLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeReleaseLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeReleaseLabel where
  toJSON DescribeReleaseLabel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ReleaseLabel" Core..=) Prelude.<$> releaseLabel
          ]
      )

instance Core.ToPath DescribeReleaseLabel where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReleaseLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReleaseLabelResponse' smart constructor.
data DescribeReleaseLabelResponse = DescribeReleaseLabelResponse'
  { -- | The pagination token. Reserved for future use. Currently set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The target release label described in the response.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The list of applications available for the target release label. @Name@
    -- is the name of the application. @Version@ is the concise version of the
    -- application.
    applications :: Prelude.Maybe [SimplifiedApplication],
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
-- 'nextToken', 'describeReleaseLabelResponse_nextToken' - The pagination token. Reserved for future use. Currently set to null.
--
-- 'releaseLabel', 'describeReleaseLabelResponse_releaseLabel' - The target release label described in the response.
--
-- 'applications', 'describeReleaseLabelResponse_applications' - The list of applications available for the target release label. @Name@
-- is the name of the application. @Version@ is the concise version of the
-- application.
--
-- 'httpStatus', 'describeReleaseLabelResponse_httpStatus' - The response's http status code.
newDescribeReleaseLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReleaseLabelResponse
newDescribeReleaseLabelResponse pHttpStatus_ =
  DescribeReleaseLabelResponse'
    { nextToken =
        Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      applications = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token. Reserved for future use. Currently set to null.
describeReleaseLabelResponse_nextToken :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe Prelude.Text)
describeReleaseLabelResponse_nextToken = Lens.lens (\DescribeReleaseLabelResponse' {nextToken} -> nextToken) (\s@DescribeReleaseLabelResponse' {} a -> s {nextToken = a} :: DescribeReleaseLabelResponse)

-- | The target release label described in the response.
describeReleaseLabelResponse_releaseLabel :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe Prelude.Text)
describeReleaseLabelResponse_releaseLabel = Lens.lens (\DescribeReleaseLabelResponse' {releaseLabel} -> releaseLabel) (\s@DescribeReleaseLabelResponse' {} a -> s {releaseLabel = a} :: DescribeReleaseLabelResponse)

-- | The list of applications available for the target release label. @Name@
-- is the name of the application. @Version@ is the concise version of the
-- application.
describeReleaseLabelResponse_applications :: Lens.Lens' DescribeReleaseLabelResponse (Prelude.Maybe [SimplifiedApplication])
describeReleaseLabelResponse_applications = Lens.lens (\DescribeReleaseLabelResponse' {applications} -> applications) (\s@DescribeReleaseLabelResponse' {} a -> s {applications = a} :: DescribeReleaseLabelResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeReleaseLabelResponse_httpStatus :: Lens.Lens' DescribeReleaseLabelResponse Prelude.Int
describeReleaseLabelResponse_httpStatus = Lens.lens (\DescribeReleaseLabelResponse' {httpStatus} -> httpStatus) (\s@DescribeReleaseLabelResponse' {} a -> s {httpStatus = a} :: DescribeReleaseLabelResponse)

instance Prelude.NFData DescribeReleaseLabelResponse
