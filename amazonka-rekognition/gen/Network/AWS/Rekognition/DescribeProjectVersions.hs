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
-- Module      : Network.AWS.Rekognition.DescribeProjectVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and describes the models in an Amazon Rekognition Custom Labels
-- project. You can specify up to 10 model versions in
-- @ProjectVersionArns@. If you don\'t specify a value, descriptions for
-- all models are returned.
--
-- This operation requires permissions to perform the
-- @rekognition:DescribeProjectVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjectVersions
  ( -- * Creating a Request
    DescribeProjectVersions (..),
    newDescribeProjectVersions,

    -- * Request Lenses
    describeProjectVersions_nextToken,
    describeProjectVersions_versionNames,
    describeProjectVersions_maxResults,
    describeProjectVersions_projectArn,

    -- * Destructuring the Response
    DescribeProjectVersionsResponse (..),
    newDescribeProjectVersionsResponse,

    -- * Response Lenses
    describeProjectVersionsResponse_nextToken,
    describeProjectVersionsResponse_projectVersionDescriptions,
    describeProjectVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeProjectVersions' smart constructor.
data DescribeProjectVersions = DescribeProjectVersions'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of model version names that you want to describe. You can add up
    -- to 10 model version names to the list. If you don\'t specify a value,
    -- all model descriptions are returned. A version name is part of a model
    -- (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the
    -- version name in the following ARN.
    -- @arn:aws:rekognition:us-east-1:123456789012:project\/getting-started\/version\/my-model.2020-01-21T09.10.15\/1234567890123@.
    versionNames :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the project that contains the models
    -- you want to describe.
    projectArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProjectVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeProjectVersions_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'versionNames', 'describeProjectVersions_versionNames' - A list of model version names that you want to describe. You can add up
-- to 10 model version names to the list. If you don\'t specify a value,
-- all model descriptions are returned. A version name is part of a model
-- (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the
-- version name in the following ARN.
-- @arn:aws:rekognition:us-east-1:123456789012:project\/getting-started\/version\/my-model.2020-01-21T09.10.15\/1234567890123@.
--
-- 'maxResults', 'describeProjectVersions_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
--
-- 'projectArn', 'describeProjectVersions_projectArn' - The Amazon Resource Name (ARN) of the project that contains the models
-- you want to describe.
newDescribeProjectVersions ::
  -- | 'projectArn'
  Core.Text ->
  DescribeProjectVersions
newDescribeProjectVersions pProjectArn_ =
  DescribeProjectVersions'
    { nextToken = Core.Nothing,
      versionNames = Core.Nothing,
      maxResults = Core.Nothing,
      projectArn = pProjectArn_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjectVersions_nextToken :: Lens.Lens' DescribeProjectVersions (Core.Maybe Core.Text)
describeProjectVersions_nextToken = Lens.lens (\DescribeProjectVersions' {nextToken} -> nextToken) (\s@DescribeProjectVersions' {} a -> s {nextToken = a} :: DescribeProjectVersions)

-- | A list of model version names that you want to describe. You can add up
-- to 10 model version names to the list. If you don\'t specify a value,
-- all model descriptions are returned. A version name is part of a model
-- (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the
-- version name in the following ARN.
-- @arn:aws:rekognition:us-east-1:123456789012:project\/getting-started\/version\/my-model.2020-01-21T09.10.15\/1234567890123@.
describeProjectVersions_versionNames :: Lens.Lens' DescribeProjectVersions (Core.Maybe (Core.NonEmpty Core.Text))
describeProjectVersions_versionNames = Lens.lens (\DescribeProjectVersions' {versionNames} -> versionNames) (\s@DescribeProjectVersions' {} a -> s {versionNames = a} :: DescribeProjectVersions) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
describeProjectVersions_maxResults :: Lens.Lens' DescribeProjectVersions (Core.Maybe Core.Natural)
describeProjectVersions_maxResults = Lens.lens (\DescribeProjectVersions' {maxResults} -> maxResults) (\s@DescribeProjectVersions' {} a -> s {maxResults = a} :: DescribeProjectVersions)

-- | The Amazon Resource Name (ARN) of the project that contains the models
-- you want to describe.
describeProjectVersions_projectArn :: Lens.Lens' DescribeProjectVersions Core.Text
describeProjectVersions_projectArn = Lens.lens (\DescribeProjectVersions' {projectArn} -> projectArn) (\s@DescribeProjectVersions' {} a -> s {projectArn = a} :: DescribeProjectVersions)

instance Core.AWSPager DescribeProjectVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeProjectVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeProjectVersionsResponse_projectVersionDescriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeProjectVersions_nextToken
          Lens..~ rs
          Lens.^? describeProjectVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeProjectVersions where
  type
    AWSResponse DescribeProjectVersions =
      DescribeProjectVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ProjectVersionDescriptions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProjectVersions

instance Core.NFData DescribeProjectVersions

instance Core.ToHeaders DescribeProjectVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DescribeProjectVersions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProjectVersions where
  toJSON DescribeProjectVersions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("VersionNames" Core..=) Core.<$> versionNames,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("ProjectArn" Core..= projectArn)
          ]
      )

instance Core.ToPath DescribeProjectVersions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProjectVersions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProjectVersionsResponse' smart constructor.
data DescribeProjectVersionsResponse = DescribeProjectVersionsResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of model descriptions. The list is sorted by the creation date
    -- and time of the model versions, latest to earliest.
    projectVersionDescriptions :: Core.Maybe [ProjectVersionDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProjectVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeProjectVersionsResponse_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'projectVersionDescriptions', 'describeProjectVersionsResponse_projectVersionDescriptions' - A list of model descriptions. The list is sorted by the creation date
-- and time of the model versions, latest to earliest.
--
-- 'httpStatus', 'describeProjectVersionsResponse_httpStatus' - The response's http status code.
newDescribeProjectVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProjectVersionsResponse
newDescribeProjectVersionsResponse pHttpStatus_ =
  DescribeProjectVersionsResponse'
    { nextToken =
        Core.Nothing,
      projectVersionDescriptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjectVersionsResponse_nextToken :: Lens.Lens' DescribeProjectVersionsResponse (Core.Maybe Core.Text)
describeProjectVersionsResponse_nextToken = Lens.lens (\DescribeProjectVersionsResponse' {nextToken} -> nextToken) (\s@DescribeProjectVersionsResponse' {} a -> s {nextToken = a} :: DescribeProjectVersionsResponse)

-- | A list of model descriptions. The list is sorted by the creation date
-- and time of the model versions, latest to earliest.
describeProjectVersionsResponse_projectVersionDescriptions :: Lens.Lens' DescribeProjectVersionsResponse (Core.Maybe [ProjectVersionDescription])
describeProjectVersionsResponse_projectVersionDescriptions = Lens.lens (\DescribeProjectVersionsResponse' {projectVersionDescriptions} -> projectVersionDescriptions) (\s@DescribeProjectVersionsResponse' {} a -> s {projectVersionDescriptions = a} :: DescribeProjectVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProjectVersionsResponse_httpStatus :: Lens.Lens' DescribeProjectVersionsResponse Core.Int
describeProjectVersionsResponse_httpStatus = Lens.lens (\DescribeProjectVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectVersionsResponse' {} a -> s {httpStatus = a} :: DescribeProjectVersionsResponse)

instance Core.NFData DescribeProjectVersionsResponse
