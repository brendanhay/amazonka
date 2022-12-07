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
-- Module      : Amazonka.EFS.DescribeReplicationConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the replication configuration for a specific file system. If a
-- file system is not specified, all of the replication configurations for
-- the Amazon Web Services account in an Amazon Web Services Region are
-- retrieved.
module Amazonka.EFS.DescribeReplicationConfigurations
  ( -- * Creating a Request
    DescribeReplicationConfigurations (..),
    newDescribeReplicationConfigurations,

    -- * Request Lenses
    describeReplicationConfigurations_nextToken,
    describeReplicationConfigurations_fileSystemId,
    describeReplicationConfigurations_maxResults,

    -- * Destructuring the Response
    DescribeReplicationConfigurationsResponse (..),
    newDescribeReplicationConfigurationsResponse,

    -- * Response Lenses
    describeReplicationConfigurationsResponse_nextToken,
    describeReplicationConfigurationsResponse_replications,
    describeReplicationConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReplicationConfigurations' smart constructor.
data DescribeReplicationConfigurations = DescribeReplicationConfigurations'
  { -- | @NextToken@ is present if the response is paginated. You can use
    -- @NextToken@ in a subsequent request to fetch the next page of output.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can retrieve the replication configuration for a specific file
    -- system by providing its file system ID.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) To limit the number of objects returned in a response, you
    -- can specify the @MaxItems@ parameter. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReplicationConfigurations_nextToken' - @NextToken@ is present if the response is paginated. You can use
-- @NextToken@ in a subsequent request to fetch the next page of output.
--
-- 'fileSystemId', 'describeReplicationConfigurations_fileSystemId' - You can retrieve the replication configuration for a specific file
-- system by providing its file system ID.
--
-- 'maxResults', 'describeReplicationConfigurations_maxResults' - (Optional) To limit the number of objects returned in a response, you
-- can specify the @MaxItems@ parameter. The default value is 100.
newDescribeReplicationConfigurations ::
  DescribeReplicationConfigurations
newDescribeReplicationConfigurations =
  DescribeReplicationConfigurations'
    { nextToken =
        Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | @NextToken@ is present if the response is paginated. You can use
-- @NextToken@ in a subsequent request to fetch the next page of output.
describeReplicationConfigurations_nextToken :: Lens.Lens' DescribeReplicationConfigurations (Prelude.Maybe Prelude.Text)
describeReplicationConfigurations_nextToken = Lens.lens (\DescribeReplicationConfigurations' {nextToken} -> nextToken) (\s@DescribeReplicationConfigurations' {} a -> s {nextToken = a} :: DescribeReplicationConfigurations)

-- | You can retrieve the replication configuration for a specific file
-- system by providing its file system ID.
describeReplicationConfigurations_fileSystemId :: Lens.Lens' DescribeReplicationConfigurations (Prelude.Maybe Prelude.Text)
describeReplicationConfigurations_fileSystemId = Lens.lens (\DescribeReplicationConfigurations' {fileSystemId} -> fileSystemId) (\s@DescribeReplicationConfigurations' {} a -> s {fileSystemId = a} :: DescribeReplicationConfigurations)

-- | (Optional) To limit the number of objects returned in a response, you
-- can specify the @MaxItems@ parameter. The default value is 100.
describeReplicationConfigurations_maxResults :: Lens.Lens' DescribeReplicationConfigurations (Prelude.Maybe Prelude.Natural)
describeReplicationConfigurations_maxResults = Lens.lens (\DescribeReplicationConfigurations' {maxResults} -> maxResults) (\s@DescribeReplicationConfigurations' {} a -> s {maxResults = a} :: DescribeReplicationConfigurations)

instance
  Core.AWSRequest
    DescribeReplicationConfigurations
  where
  type
    AWSResponse DescribeReplicationConfigurations =
      DescribeReplicationConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationConfigurationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Replications" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationConfigurations
  where
  hashWithSalt
    _salt
    DescribeReplicationConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` fileSystemId
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeReplicationConfigurations
  where
  rnf DescribeReplicationConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    DescribeReplicationConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeReplicationConfigurations
  where
  toPath =
    Prelude.const
      "/2015-02-01/file-systems/replication-configurations"

instance
  Data.ToQuery
    DescribeReplicationConfigurations
  where
  toQuery DescribeReplicationConfigurations' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "FileSystemId" Data.=: fileSystemId,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newDescribeReplicationConfigurationsResponse' smart constructor.
data DescribeReplicationConfigurationsResponse = DescribeReplicationConfigurationsResponse'
  { -- | You can use the @NextToken@ from the previous response in a subsequent
    -- request to fetch the additional descriptions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of replication configurations that is returned.
    replications :: Prelude.Maybe [ReplicationConfigurationDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReplicationConfigurationsResponse_nextToken' - You can use the @NextToken@ from the previous response in a subsequent
-- request to fetch the additional descriptions.
--
-- 'replications', 'describeReplicationConfigurationsResponse_replications' - The collection of replication configurations that is returned.
--
-- 'httpStatus', 'describeReplicationConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeReplicationConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationConfigurationsResponse
newDescribeReplicationConfigurationsResponse
  pHttpStatus_ =
    DescribeReplicationConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        replications = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | You can use the @NextToken@ from the previous response in a subsequent
-- request to fetch the additional descriptions.
describeReplicationConfigurationsResponse_nextToken :: Lens.Lens' DescribeReplicationConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeReplicationConfigurationsResponse_nextToken = Lens.lens (\DescribeReplicationConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeReplicationConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeReplicationConfigurationsResponse)

-- | The collection of replication configurations that is returned.
describeReplicationConfigurationsResponse_replications :: Lens.Lens' DescribeReplicationConfigurationsResponse (Prelude.Maybe [ReplicationConfigurationDescription])
describeReplicationConfigurationsResponse_replications = Lens.lens (\DescribeReplicationConfigurationsResponse' {replications} -> replications) (\s@DescribeReplicationConfigurationsResponse' {} a -> s {replications = a} :: DescribeReplicationConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationConfigurationsResponse_httpStatus :: Lens.Lens' DescribeReplicationConfigurationsResponse Prelude.Int
describeReplicationConfigurationsResponse_httpStatus = Lens.lens (\DescribeReplicationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationConfigurationsResponse)

instance
  Prelude.NFData
    DescribeReplicationConfigurationsResponse
  where
  rnf DescribeReplicationConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf replications
      `Prelude.seq` Prelude.rnf httpStatus
