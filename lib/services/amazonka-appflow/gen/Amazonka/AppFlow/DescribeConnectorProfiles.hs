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
-- Module      : Amazonka.AppFlow.DescribeConnectorProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @connector-profile@ details matching the provided
-- @connector-profile@ names and @connector-types@. Both input lists are
-- optional, and you can use them to filter the result.
--
-- If no names or @connector-types@ are provided, returns all connector
-- profiles in a paginated form. If there is no match, this operation
-- returns an empty list.
module Amazonka.AppFlow.DescribeConnectorProfiles
  ( -- * Creating a Request
    DescribeConnectorProfiles (..),
    newDescribeConnectorProfiles,

    -- * Request Lenses
    describeConnectorProfiles_connectorLabel,
    describeConnectorProfiles_connectorProfileNames,
    describeConnectorProfiles_connectorType,
    describeConnectorProfiles_maxResults,
    describeConnectorProfiles_nextToken,

    -- * Destructuring the Response
    DescribeConnectorProfilesResponse (..),
    newDescribeConnectorProfilesResponse,

    -- * Response Lenses
    describeConnectorProfilesResponse_connectorProfileDetails,
    describeConnectorProfilesResponse_nextToken,
    describeConnectorProfilesResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConnectorProfiles' smart constructor.
data DescribeConnectorProfiles = DescribeConnectorProfiles'
  { -- | The name of the connector. The name is unique for each
    -- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
    -- if calling for CUSTOMCONNECTOR connector type\/.
    connectorLabel :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account.
    connectorProfileNames :: Prelude.Maybe [Prelude.Text],
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: Prelude.Maybe ConnectorType,
    -- | Specifies the maximum number of items that should be returned in the
    -- result set. The default for @maxResults@ is 20 (for all paginated API
    -- operations).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorLabel', 'describeConnectorProfiles_connectorLabel' - The name of the connector. The name is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
-- if calling for CUSTOMCONNECTOR connector type\/.
--
-- 'connectorProfileNames', 'describeConnectorProfiles_connectorProfileNames' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
--
-- 'connectorType', 'describeConnectorProfiles_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
--
-- 'maxResults', 'describeConnectorProfiles_maxResults' - Specifies the maximum number of items that should be returned in the
-- result set. The default for @maxResults@ is 20 (for all paginated API
-- operations).
--
-- 'nextToken', 'describeConnectorProfiles_nextToken' - The pagination token for the next page of data.
newDescribeConnectorProfiles ::
  DescribeConnectorProfiles
newDescribeConnectorProfiles =
  DescribeConnectorProfiles'
    { connectorLabel =
        Prelude.Nothing,
      connectorProfileNames = Prelude.Nothing,
      connectorType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the connector. The name is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
-- if calling for CUSTOMCONNECTOR connector type\/.
describeConnectorProfiles_connectorLabel :: Lens.Lens' DescribeConnectorProfiles (Prelude.Maybe Prelude.Text)
describeConnectorProfiles_connectorLabel = Lens.lens (\DescribeConnectorProfiles' {connectorLabel} -> connectorLabel) (\s@DescribeConnectorProfiles' {} a -> s {connectorLabel = a} :: DescribeConnectorProfiles)

-- | The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
describeConnectorProfiles_connectorProfileNames :: Lens.Lens' DescribeConnectorProfiles (Prelude.Maybe [Prelude.Text])
describeConnectorProfiles_connectorProfileNames = Lens.lens (\DescribeConnectorProfiles' {connectorProfileNames} -> connectorProfileNames) (\s@DescribeConnectorProfiles' {} a -> s {connectorProfileNames = a} :: DescribeConnectorProfiles) Prelude.. Lens.mapping Lens.coerced

-- | The type of connector, such as Salesforce, Amplitude, and so on.
describeConnectorProfiles_connectorType :: Lens.Lens' DescribeConnectorProfiles (Prelude.Maybe ConnectorType)
describeConnectorProfiles_connectorType = Lens.lens (\DescribeConnectorProfiles' {connectorType} -> connectorType) (\s@DescribeConnectorProfiles' {} a -> s {connectorType = a} :: DescribeConnectorProfiles)

-- | Specifies the maximum number of items that should be returned in the
-- result set. The default for @maxResults@ is 20 (for all paginated API
-- operations).
describeConnectorProfiles_maxResults :: Lens.Lens' DescribeConnectorProfiles (Prelude.Maybe Prelude.Natural)
describeConnectorProfiles_maxResults = Lens.lens (\DescribeConnectorProfiles' {maxResults} -> maxResults) (\s@DescribeConnectorProfiles' {} a -> s {maxResults = a} :: DescribeConnectorProfiles)

-- | The pagination token for the next page of data.
describeConnectorProfiles_nextToken :: Lens.Lens' DescribeConnectorProfiles (Prelude.Maybe Prelude.Text)
describeConnectorProfiles_nextToken = Lens.lens (\DescribeConnectorProfiles' {nextToken} -> nextToken) (\s@DescribeConnectorProfiles' {} a -> s {nextToken = a} :: DescribeConnectorProfiles)

instance Core.AWSRequest DescribeConnectorProfiles where
  type
    AWSResponse DescribeConnectorProfiles =
      DescribeConnectorProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectorProfilesResponse'
            Prelude.<$> ( x
                            Data..?> "connectorProfileDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnectorProfiles where
  hashWithSalt _salt DescribeConnectorProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` connectorLabel
      `Prelude.hashWithSalt` connectorProfileNames
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeConnectorProfiles where
  rnf DescribeConnectorProfiles' {..} =
    Prelude.rnf connectorLabel
      `Prelude.seq` Prelude.rnf connectorProfileNames
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeConnectorProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnectorProfiles where
  toJSON DescribeConnectorProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectorLabel" Data..=)
              Prelude.<$> connectorLabel,
            ("connectorProfileNames" Data..=)
              Prelude.<$> connectorProfileNames,
            ("connectorType" Data..=) Prelude.<$> connectorType,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeConnectorProfiles where
  toPath = Prelude.const "/describe-connector-profiles"

instance Data.ToQuery DescribeConnectorProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorProfilesResponse' smart constructor.
data DescribeConnectorProfilesResponse = DescribeConnectorProfilesResponse'
  { -- | Returns information about the connector profiles associated with the
    -- flow.
    connectorProfileDetails :: Prelude.Maybe [ConnectorProfile],
    -- | The pagination token for the next page of data. If @nextToken=null@,
    -- this means that all records have been fetched.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorProfileDetails', 'describeConnectorProfilesResponse_connectorProfileDetails' - Returns information about the connector profiles associated with the
-- flow.
--
-- 'nextToken', 'describeConnectorProfilesResponse_nextToken' - The pagination token for the next page of data. If @nextToken=null@,
-- this means that all records have been fetched.
--
-- 'httpStatus', 'describeConnectorProfilesResponse_httpStatus' - The response's http status code.
newDescribeConnectorProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectorProfilesResponse
newDescribeConnectorProfilesResponse pHttpStatus_ =
  DescribeConnectorProfilesResponse'
    { connectorProfileDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns information about the connector profiles associated with the
-- flow.
describeConnectorProfilesResponse_connectorProfileDetails :: Lens.Lens' DescribeConnectorProfilesResponse (Prelude.Maybe [ConnectorProfile])
describeConnectorProfilesResponse_connectorProfileDetails = Lens.lens (\DescribeConnectorProfilesResponse' {connectorProfileDetails} -> connectorProfileDetails) (\s@DescribeConnectorProfilesResponse' {} a -> s {connectorProfileDetails = a} :: DescribeConnectorProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the next page of data. If @nextToken=null@,
-- this means that all records have been fetched.
describeConnectorProfilesResponse_nextToken :: Lens.Lens' DescribeConnectorProfilesResponse (Prelude.Maybe Prelude.Text)
describeConnectorProfilesResponse_nextToken = Lens.lens (\DescribeConnectorProfilesResponse' {nextToken} -> nextToken) (\s@DescribeConnectorProfilesResponse' {} a -> s {nextToken = a} :: DescribeConnectorProfilesResponse)

-- | The response's http status code.
describeConnectorProfilesResponse_httpStatus :: Lens.Lens' DescribeConnectorProfilesResponse Prelude.Int
describeConnectorProfilesResponse_httpStatus = Lens.lens (\DescribeConnectorProfilesResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorProfilesResponse' {} a -> s {httpStatus = a} :: DescribeConnectorProfilesResponse)

instance
  Prelude.NFData
    DescribeConnectorProfilesResponse
  where
  rnf DescribeConnectorProfilesResponse' {..} =
    Prelude.rnf connectorProfileDetails
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
