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
-- Module      : Amazonka.Rekognition.DescribeProjectVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and describes the versions of a model in an Amazon Rekognition
-- Custom Labels project. You can specify up to 10 model versions in
-- @ProjectVersionArns@. If you don\'t specify a value, descriptions for
-- all model versions in the project are returned.
--
-- This operation requires permissions to perform the
-- @rekognition:DescribeProjectVersions@ action.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.DescribeProjectVersions
  ( -- * Creating a Request
    DescribeProjectVersions (..),
    newDescribeProjectVersions,

    -- * Request Lenses
    describeProjectVersions_maxResults,
    describeProjectVersions_nextToken,
    describeProjectVersions_versionNames,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProjectVersions' smart constructor.
data DescribeProjectVersions = DescribeProjectVersions'
  { -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of model version names that you want to describe. You can add up
    -- to 10 model version names to the list. If you don\'t specify a value,
    -- all model descriptions are returned. A version name is part of a model
    -- (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the
    -- version name in the following ARN.
    -- @arn:aws:rekognition:us-east-1:123456789012:project\/getting-started\/version\/my-model.2020-01-21T09.10.15\/1234567890123@.
    versionNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the project that contains the models
    -- you want to describe.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProjectVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeProjectVersions_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
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
-- 'projectArn', 'describeProjectVersions_projectArn' - The Amazon Resource Name (ARN) of the project that contains the models
-- you want to describe.
newDescribeProjectVersions ::
  -- | 'projectArn'
  Prelude.Text ->
  DescribeProjectVersions
newDescribeProjectVersions pProjectArn_ =
  DescribeProjectVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      versionNames = Prelude.Nothing,
      projectArn = pProjectArn_
    }

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
describeProjectVersions_maxResults :: Lens.Lens' DescribeProjectVersions (Prelude.Maybe Prelude.Natural)
describeProjectVersions_maxResults = Lens.lens (\DescribeProjectVersions' {maxResults} -> maxResults) (\s@DescribeProjectVersions' {} a -> s {maxResults = a} :: DescribeProjectVersions)

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjectVersions_nextToken :: Lens.Lens' DescribeProjectVersions (Prelude.Maybe Prelude.Text)
describeProjectVersions_nextToken = Lens.lens (\DescribeProjectVersions' {nextToken} -> nextToken) (\s@DescribeProjectVersions' {} a -> s {nextToken = a} :: DescribeProjectVersions)

-- | A list of model version names that you want to describe. You can add up
-- to 10 model version names to the list. If you don\'t specify a value,
-- all model descriptions are returned. A version name is part of a model
-- (ProjectVersion) ARN. For example, @my-model.2020-01-21T09.10.15@ is the
-- version name in the following ARN.
-- @arn:aws:rekognition:us-east-1:123456789012:project\/getting-started\/version\/my-model.2020-01-21T09.10.15\/1234567890123@.
describeProjectVersions_versionNames :: Lens.Lens' DescribeProjectVersions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeProjectVersions_versionNames = Lens.lens (\DescribeProjectVersions' {versionNames} -> versionNames) (\s@DescribeProjectVersions' {} a -> s {versionNames = a} :: DescribeProjectVersions) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the project that contains the models
-- you want to describe.
describeProjectVersions_projectArn :: Lens.Lens' DescribeProjectVersions Prelude.Text
describeProjectVersions_projectArn = Lens.lens (\DescribeProjectVersions' {projectArn} -> projectArn) (\s@DescribeProjectVersions' {} a -> s {projectArn = a} :: DescribeProjectVersions)

instance Core.AWSPager DescribeProjectVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeProjectVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeProjectVersionsResponse_projectVersionDescriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeProjectVersions_nextToken
          Lens..~ rs
          Lens.^? describeProjectVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeProjectVersions where
  type
    AWSResponse DescribeProjectVersions =
      DescribeProjectVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ProjectVersionDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProjectVersions where
  hashWithSalt _salt DescribeProjectVersions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` versionNames
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData DescribeProjectVersions where
  rnf DescribeProjectVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versionNames
      `Prelude.seq` Prelude.rnf projectArn

instance Data.ToHeaders DescribeProjectVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DescribeProjectVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProjectVersions where
  toJSON DescribeProjectVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("VersionNames" Data..=) Prelude.<$> versionNames,
            Prelude.Just ("ProjectArn" Data..= projectArn)
          ]
      )

instance Data.ToPath DescribeProjectVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProjectVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectVersionsResponse' smart constructor.
data DescribeProjectVersionsResponse = DescribeProjectVersionsResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of model descriptions. The list is sorted by the creation date
    -- and time of the model versions, latest to earliest.
    projectVersionDescriptions :: Prelude.Maybe [ProjectVersionDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeProjectVersionsResponse
newDescribeProjectVersionsResponse pHttpStatus_ =
  DescribeProjectVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      projectVersionDescriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjectVersionsResponse_nextToken :: Lens.Lens' DescribeProjectVersionsResponse (Prelude.Maybe Prelude.Text)
describeProjectVersionsResponse_nextToken = Lens.lens (\DescribeProjectVersionsResponse' {nextToken} -> nextToken) (\s@DescribeProjectVersionsResponse' {} a -> s {nextToken = a} :: DescribeProjectVersionsResponse)

-- | A list of model descriptions. The list is sorted by the creation date
-- and time of the model versions, latest to earliest.
describeProjectVersionsResponse_projectVersionDescriptions :: Lens.Lens' DescribeProjectVersionsResponse (Prelude.Maybe [ProjectVersionDescription])
describeProjectVersionsResponse_projectVersionDescriptions = Lens.lens (\DescribeProjectVersionsResponse' {projectVersionDescriptions} -> projectVersionDescriptions) (\s@DescribeProjectVersionsResponse' {} a -> s {projectVersionDescriptions = a} :: DescribeProjectVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProjectVersionsResponse_httpStatus :: Lens.Lens' DescribeProjectVersionsResponse Prelude.Int
describeProjectVersionsResponse_httpStatus = Lens.lens (\DescribeProjectVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectVersionsResponse' {} a -> s {httpStatus = a} :: DescribeProjectVersionsResponse)

instance
  Prelude.NFData
    DescribeProjectVersionsResponse
  where
  rnf DescribeProjectVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projectVersionDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
