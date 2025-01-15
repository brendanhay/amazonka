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
-- Module      : Amazonka.FSx.DescribeStorageVirtualMachines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Amazon FSx for NetApp ONTAP storage virtual
-- machines (SVMs).
--
-- This operation returns paginated results.
module Amazonka.FSx.DescribeStorageVirtualMachines
  ( -- * Creating a Request
    DescribeStorageVirtualMachines (..),
    newDescribeStorageVirtualMachines,

    -- * Request Lenses
    describeStorageVirtualMachines_filters,
    describeStorageVirtualMachines_maxResults,
    describeStorageVirtualMachines_nextToken,
    describeStorageVirtualMachines_storageVirtualMachineIds,

    -- * Destructuring the Response
    DescribeStorageVirtualMachinesResponse (..),
    newDescribeStorageVirtualMachinesResponse,

    -- * Response Lenses
    describeStorageVirtualMachinesResponse_nextToken,
    describeStorageVirtualMachinesResponse_storageVirtualMachines,
    describeStorageVirtualMachinesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStorageVirtualMachines' smart constructor.
data DescribeStorageVirtualMachines = DescribeStorageVirtualMachines'
  { -- | Enter a filter name:value pair to view a select set of SVMs.
    filters :: Prelude.Maybe [StorageVirtualMachineFilter],
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Enter the ID of one or more SVMs that you want to view.
    storageVirtualMachineIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageVirtualMachines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeStorageVirtualMachines_filters' - Enter a filter name:value pair to view a select set of SVMs.
--
-- 'maxResults', 'describeStorageVirtualMachines_maxResults' - Undocumented member.
--
-- 'nextToken', 'describeStorageVirtualMachines_nextToken' - Undocumented member.
--
-- 'storageVirtualMachineIds', 'describeStorageVirtualMachines_storageVirtualMachineIds' - Enter the ID of one or more SVMs that you want to view.
newDescribeStorageVirtualMachines ::
  DescribeStorageVirtualMachines
newDescribeStorageVirtualMachines =
  DescribeStorageVirtualMachines'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      storageVirtualMachineIds = Prelude.Nothing
    }

-- | Enter a filter name:value pair to view a select set of SVMs.
describeStorageVirtualMachines_filters :: Lens.Lens' DescribeStorageVirtualMachines (Prelude.Maybe [StorageVirtualMachineFilter])
describeStorageVirtualMachines_filters = Lens.lens (\DescribeStorageVirtualMachines' {filters} -> filters) (\s@DescribeStorageVirtualMachines' {} a -> s {filters = a} :: DescribeStorageVirtualMachines) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeStorageVirtualMachines_maxResults :: Lens.Lens' DescribeStorageVirtualMachines (Prelude.Maybe Prelude.Natural)
describeStorageVirtualMachines_maxResults = Lens.lens (\DescribeStorageVirtualMachines' {maxResults} -> maxResults) (\s@DescribeStorageVirtualMachines' {} a -> s {maxResults = a} :: DescribeStorageVirtualMachines)

-- | Undocumented member.
describeStorageVirtualMachines_nextToken :: Lens.Lens' DescribeStorageVirtualMachines (Prelude.Maybe Prelude.Text)
describeStorageVirtualMachines_nextToken = Lens.lens (\DescribeStorageVirtualMachines' {nextToken} -> nextToken) (\s@DescribeStorageVirtualMachines' {} a -> s {nextToken = a} :: DescribeStorageVirtualMachines)

-- | Enter the ID of one or more SVMs that you want to view.
describeStorageVirtualMachines_storageVirtualMachineIds :: Lens.Lens' DescribeStorageVirtualMachines (Prelude.Maybe [Prelude.Text])
describeStorageVirtualMachines_storageVirtualMachineIds = Lens.lens (\DescribeStorageVirtualMachines' {storageVirtualMachineIds} -> storageVirtualMachineIds) (\s@DescribeStorageVirtualMachines' {} a -> s {storageVirtualMachineIds = a} :: DescribeStorageVirtualMachines) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeStorageVirtualMachines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStorageVirtualMachinesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStorageVirtualMachinesResponse_storageVirtualMachines
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeStorageVirtualMachines_nextToken
              Lens..~ rs
              Lens.^? describeStorageVirtualMachinesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeStorageVirtualMachines
  where
  type
    AWSResponse DescribeStorageVirtualMachines =
      DescribeStorageVirtualMachinesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorageVirtualMachinesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "StorageVirtualMachines"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeStorageVirtualMachines
  where
  hashWithSalt
    _salt
    DescribeStorageVirtualMachines' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` storageVirtualMachineIds

instance
  Prelude.NFData
    DescribeStorageVirtualMachines
  where
  rnf DescribeStorageVirtualMachines' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf storageVirtualMachineIds

instance
  Data.ToHeaders
    DescribeStorageVirtualMachines
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DescribeStorageVirtualMachines" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStorageVirtualMachines where
  toJSON DescribeStorageVirtualMachines' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StorageVirtualMachineIds" Data..=)
              Prelude.<$> storageVirtualMachineIds
          ]
      )

instance Data.ToPath DescribeStorageVirtualMachines where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStorageVirtualMachines where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStorageVirtualMachinesResponse' smart constructor.
data DescribeStorageVirtualMachinesResponse = DescribeStorageVirtualMachinesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returned after a successful @DescribeStorageVirtualMachines@ operation,
    -- describing each SVM.
    storageVirtualMachines :: Prelude.Maybe [StorageVirtualMachine],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageVirtualMachinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStorageVirtualMachinesResponse_nextToken' - Undocumented member.
--
-- 'storageVirtualMachines', 'describeStorageVirtualMachinesResponse_storageVirtualMachines' - Returned after a successful @DescribeStorageVirtualMachines@ operation,
-- describing each SVM.
--
-- 'httpStatus', 'describeStorageVirtualMachinesResponse_httpStatus' - The response's http status code.
newDescribeStorageVirtualMachinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStorageVirtualMachinesResponse
newDescribeStorageVirtualMachinesResponse
  pHttpStatus_ =
    DescribeStorageVirtualMachinesResponse'
      { nextToken =
          Prelude.Nothing,
        storageVirtualMachines =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeStorageVirtualMachinesResponse_nextToken :: Lens.Lens' DescribeStorageVirtualMachinesResponse (Prelude.Maybe Prelude.Text)
describeStorageVirtualMachinesResponse_nextToken = Lens.lens (\DescribeStorageVirtualMachinesResponse' {nextToken} -> nextToken) (\s@DescribeStorageVirtualMachinesResponse' {} a -> s {nextToken = a} :: DescribeStorageVirtualMachinesResponse)

-- | Returned after a successful @DescribeStorageVirtualMachines@ operation,
-- describing each SVM.
describeStorageVirtualMachinesResponse_storageVirtualMachines :: Lens.Lens' DescribeStorageVirtualMachinesResponse (Prelude.Maybe [StorageVirtualMachine])
describeStorageVirtualMachinesResponse_storageVirtualMachines = Lens.lens (\DescribeStorageVirtualMachinesResponse' {storageVirtualMachines} -> storageVirtualMachines) (\s@DescribeStorageVirtualMachinesResponse' {} a -> s {storageVirtualMachines = a} :: DescribeStorageVirtualMachinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStorageVirtualMachinesResponse_httpStatus :: Lens.Lens' DescribeStorageVirtualMachinesResponse Prelude.Int
describeStorageVirtualMachinesResponse_httpStatus = Lens.lens (\DescribeStorageVirtualMachinesResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageVirtualMachinesResponse' {} a -> s {httpStatus = a} :: DescribeStorageVirtualMachinesResponse)

instance
  Prelude.NFData
    DescribeStorageVirtualMachinesResponse
  where
  rnf DescribeStorageVirtualMachinesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf storageVirtualMachines `Prelude.seq`
        Prelude.rnf httpStatus
