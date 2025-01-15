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
-- Module      : Amazonka.AppStream.DescribeApplicationFleetAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more application fleet
-- associations. Either ApplicationArn or FleetName must be specified.
module Amazonka.AppStream.DescribeApplicationFleetAssociations
  ( -- * Creating a Request
    DescribeApplicationFleetAssociations (..),
    newDescribeApplicationFleetAssociations,

    -- * Request Lenses
    describeApplicationFleetAssociations_applicationArn,
    describeApplicationFleetAssociations_fleetName,
    describeApplicationFleetAssociations_maxResults,
    describeApplicationFleetAssociations_nextToken,

    -- * Destructuring the Response
    DescribeApplicationFleetAssociationsResponse (..),
    newDescribeApplicationFleetAssociationsResponse,

    -- * Response Lenses
    describeApplicationFleetAssociationsResponse_applicationFleetAssociations,
    describeApplicationFleetAssociationsResponse_nextToken,
    describeApplicationFleetAssociationsResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplicationFleetAssociations' smart constructor.
data DescribeApplicationFleetAssociations = DescribeApplicationFleetAssociations'
  { -- | The ARN of the application.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet.
    fleetName :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationFleetAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'describeApplicationFleetAssociations_applicationArn' - The ARN of the application.
--
-- 'fleetName', 'describeApplicationFleetAssociations_fleetName' - The name of the fleet.
--
-- 'maxResults', 'describeApplicationFleetAssociations_maxResults' - The maximum size of each page of results.
--
-- 'nextToken', 'describeApplicationFleetAssociations_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newDescribeApplicationFleetAssociations ::
  DescribeApplicationFleetAssociations
newDescribeApplicationFleetAssociations =
  DescribeApplicationFleetAssociations'
    { applicationArn =
        Prelude.Nothing,
      fleetName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ARN of the application.
describeApplicationFleetAssociations_applicationArn :: Lens.Lens' DescribeApplicationFleetAssociations (Prelude.Maybe Prelude.Text)
describeApplicationFleetAssociations_applicationArn = Lens.lens (\DescribeApplicationFleetAssociations' {applicationArn} -> applicationArn) (\s@DescribeApplicationFleetAssociations' {} a -> s {applicationArn = a} :: DescribeApplicationFleetAssociations)

-- | The name of the fleet.
describeApplicationFleetAssociations_fleetName :: Lens.Lens' DescribeApplicationFleetAssociations (Prelude.Maybe Prelude.Text)
describeApplicationFleetAssociations_fleetName = Lens.lens (\DescribeApplicationFleetAssociations' {fleetName} -> fleetName) (\s@DescribeApplicationFleetAssociations' {} a -> s {fleetName = a} :: DescribeApplicationFleetAssociations)

-- | The maximum size of each page of results.
describeApplicationFleetAssociations_maxResults :: Lens.Lens' DescribeApplicationFleetAssociations (Prelude.Maybe Prelude.Int)
describeApplicationFleetAssociations_maxResults = Lens.lens (\DescribeApplicationFleetAssociations' {maxResults} -> maxResults) (\s@DescribeApplicationFleetAssociations' {} a -> s {maxResults = a} :: DescribeApplicationFleetAssociations)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeApplicationFleetAssociations_nextToken :: Lens.Lens' DescribeApplicationFleetAssociations (Prelude.Maybe Prelude.Text)
describeApplicationFleetAssociations_nextToken = Lens.lens (\DescribeApplicationFleetAssociations' {nextToken} -> nextToken) (\s@DescribeApplicationFleetAssociations' {} a -> s {nextToken = a} :: DescribeApplicationFleetAssociations)

instance
  Core.AWSRequest
    DescribeApplicationFleetAssociations
  where
  type
    AWSResponse DescribeApplicationFleetAssociations =
      DescribeApplicationFleetAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationFleetAssociationsResponse'
            Prelude.<$> (x Data..?> "ApplicationFleetAssociations")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeApplicationFleetAssociations
  where
  hashWithSalt
    _salt
    DescribeApplicationFleetAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` applicationArn
        `Prelude.hashWithSalt` fleetName
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeApplicationFleetAssociations
  where
  rnf DescribeApplicationFleetAssociations' {..} =
    Prelude.rnf applicationArn `Prelude.seq`
      Prelude.rnf fleetName `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeApplicationFleetAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeApplicationFleetAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeApplicationFleetAssociations
  where
  toJSON DescribeApplicationFleetAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationArn" Data..=)
              Prelude.<$> applicationArn,
            ("FleetName" Data..=) Prelude.<$> fleetName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribeApplicationFleetAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeApplicationFleetAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationFleetAssociationsResponse' smart constructor.
data DescribeApplicationFleetAssociationsResponse = DescribeApplicationFleetAssociationsResponse'
  { -- | The application fleet associations in the list.
    applicationFleetAssociations :: Prelude.Maybe (Prelude.NonEmpty ApplicationFleetAssociation),
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationFleetAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationFleetAssociations', 'describeApplicationFleetAssociationsResponse_applicationFleetAssociations' - The application fleet associations in the list.
--
-- 'nextToken', 'describeApplicationFleetAssociationsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'httpStatus', 'describeApplicationFleetAssociationsResponse_httpStatus' - The response's http status code.
newDescribeApplicationFleetAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationFleetAssociationsResponse
newDescribeApplicationFleetAssociationsResponse
  pHttpStatus_ =
    DescribeApplicationFleetAssociationsResponse'
      { applicationFleetAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The application fleet associations in the list.
describeApplicationFleetAssociationsResponse_applicationFleetAssociations :: Lens.Lens' DescribeApplicationFleetAssociationsResponse (Prelude.Maybe (Prelude.NonEmpty ApplicationFleetAssociation))
describeApplicationFleetAssociationsResponse_applicationFleetAssociations = Lens.lens (\DescribeApplicationFleetAssociationsResponse' {applicationFleetAssociations} -> applicationFleetAssociations) (\s@DescribeApplicationFleetAssociationsResponse' {} a -> s {applicationFleetAssociations = a} :: DescribeApplicationFleetAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeApplicationFleetAssociationsResponse_nextToken :: Lens.Lens' DescribeApplicationFleetAssociationsResponse (Prelude.Maybe Prelude.Text)
describeApplicationFleetAssociationsResponse_nextToken = Lens.lens (\DescribeApplicationFleetAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeApplicationFleetAssociationsResponse' {} a -> s {nextToken = a} :: DescribeApplicationFleetAssociationsResponse)

-- | The response's http status code.
describeApplicationFleetAssociationsResponse_httpStatus :: Lens.Lens' DescribeApplicationFleetAssociationsResponse Prelude.Int
describeApplicationFleetAssociationsResponse_httpStatus = Lens.lens (\DescribeApplicationFleetAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationFleetAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationFleetAssociationsResponse)

instance
  Prelude.NFData
    DescribeApplicationFleetAssociationsResponse
  where
  rnf DescribeApplicationFleetAssociationsResponse' {..} =
    Prelude.rnf applicationFleetAssociations `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
