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
-- Module      : Amazonka.Outposts.GetOutpostInstanceTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the instance types for the specified Outpost.
module Amazonka.Outposts.GetOutpostInstanceTypes
  ( -- * Creating a Request
    GetOutpostInstanceTypes (..),
    newGetOutpostInstanceTypes,

    -- * Request Lenses
    getOutpostInstanceTypes_nextToken,
    getOutpostInstanceTypes_maxResults,
    getOutpostInstanceTypes_outpostId,

    -- * Destructuring the Response
    GetOutpostInstanceTypesResponse (..),
    newGetOutpostInstanceTypesResponse,

    -- * Response Lenses
    getOutpostInstanceTypesResponse_nextToken,
    getOutpostInstanceTypesResponse_outpostId,
    getOutpostInstanceTypesResponse_outpostArn,
    getOutpostInstanceTypesResponse_instanceTypes,
    getOutpostInstanceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOutpostInstanceTypes' smart constructor.
data GetOutpostInstanceTypes = GetOutpostInstanceTypes'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOutpostInstanceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOutpostInstanceTypes_nextToken' - Undocumented member.
--
-- 'maxResults', 'getOutpostInstanceTypes_maxResults' - Undocumented member.
--
-- 'outpostId', 'getOutpostInstanceTypes_outpostId' - The ID or the Amazon Resource Name (ARN) of the Outpost.
newGetOutpostInstanceTypes ::
  -- | 'outpostId'
  Prelude.Text ->
  GetOutpostInstanceTypes
newGetOutpostInstanceTypes pOutpostId_ =
  GetOutpostInstanceTypes'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      outpostId = pOutpostId_
    }

-- | Undocumented member.
getOutpostInstanceTypes_nextToken :: Lens.Lens' GetOutpostInstanceTypes (Prelude.Maybe Prelude.Text)
getOutpostInstanceTypes_nextToken = Lens.lens (\GetOutpostInstanceTypes' {nextToken} -> nextToken) (\s@GetOutpostInstanceTypes' {} a -> s {nextToken = a} :: GetOutpostInstanceTypes)

-- | Undocumented member.
getOutpostInstanceTypes_maxResults :: Lens.Lens' GetOutpostInstanceTypes (Prelude.Maybe Prelude.Natural)
getOutpostInstanceTypes_maxResults = Lens.lens (\GetOutpostInstanceTypes' {maxResults} -> maxResults) (\s@GetOutpostInstanceTypes' {} a -> s {maxResults = a} :: GetOutpostInstanceTypes)

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
getOutpostInstanceTypes_outpostId :: Lens.Lens' GetOutpostInstanceTypes Prelude.Text
getOutpostInstanceTypes_outpostId = Lens.lens (\GetOutpostInstanceTypes' {outpostId} -> outpostId) (\s@GetOutpostInstanceTypes' {} a -> s {outpostId = a} :: GetOutpostInstanceTypes)

instance Core.AWSRequest GetOutpostInstanceTypes where
  type
    AWSResponse GetOutpostInstanceTypes =
      GetOutpostInstanceTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOutpostInstanceTypesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "OutpostId")
            Prelude.<*> (x Core..?> "OutpostArn")
            Prelude.<*> (x Core..?> "InstanceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOutpostInstanceTypes where
  hashWithSalt _salt GetOutpostInstanceTypes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` outpostId

instance Prelude.NFData GetOutpostInstanceTypes where
  rnf GetOutpostInstanceTypes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf outpostId

instance Core.ToHeaders GetOutpostInstanceTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetOutpostInstanceTypes where
  toPath GetOutpostInstanceTypes' {..} =
    Prelude.mconcat
      ["/outposts/", Core.toBS outpostId, "/instanceTypes"]

instance Core.ToQuery GetOutpostInstanceTypes where
  toQuery GetOutpostInstanceTypes' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetOutpostInstanceTypesResponse' smart constructor.
data GetOutpostInstanceTypesResponse = GetOutpostInstanceTypesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    outpostArn :: Prelude.Maybe Prelude.Text,
    instanceTypes :: Prelude.Maybe [InstanceTypeItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOutpostInstanceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOutpostInstanceTypesResponse_nextToken' - Undocumented member.
--
-- 'outpostId', 'getOutpostInstanceTypesResponse_outpostId' - The ID of the Outpost.
--
-- 'outpostArn', 'getOutpostInstanceTypesResponse_outpostArn' - Undocumented member.
--
-- 'instanceTypes', 'getOutpostInstanceTypesResponse_instanceTypes' - Undocumented member.
--
-- 'httpStatus', 'getOutpostInstanceTypesResponse_httpStatus' - The response's http status code.
newGetOutpostInstanceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOutpostInstanceTypesResponse
newGetOutpostInstanceTypesResponse pHttpStatus_ =
  GetOutpostInstanceTypesResponse'
    { nextToken =
        Prelude.Nothing,
      outpostId = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getOutpostInstanceTypesResponse_nextToken :: Lens.Lens' GetOutpostInstanceTypesResponse (Prelude.Maybe Prelude.Text)
getOutpostInstanceTypesResponse_nextToken = Lens.lens (\GetOutpostInstanceTypesResponse' {nextToken} -> nextToken) (\s@GetOutpostInstanceTypesResponse' {} a -> s {nextToken = a} :: GetOutpostInstanceTypesResponse)

-- | The ID of the Outpost.
getOutpostInstanceTypesResponse_outpostId :: Lens.Lens' GetOutpostInstanceTypesResponse (Prelude.Maybe Prelude.Text)
getOutpostInstanceTypesResponse_outpostId = Lens.lens (\GetOutpostInstanceTypesResponse' {outpostId} -> outpostId) (\s@GetOutpostInstanceTypesResponse' {} a -> s {outpostId = a} :: GetOutpostInstanceTypesResponse)

-- | Undocumented member.
getOutpostInstanceTypesResponse_outpostArn :: Lens.Lens' GetOutpostInstanceTypesResponse (Prelude.Maybe Prelude.Text)
getOutpostInstanceTypesResponse_outpostArn = Lens.lens (\GetOutpostInstanceTypesResponse' {outpostArn} -> outpostArn) (\s@GetOutpostInstanceTypesResponse' {} a -> s {outpostArn = a} :: GetOutpostInstanceTypesResponse)

-- | Undocumented member.
getOutpostInstanceTypesResponse_instanceTypes :: Lens.Lens' GetOutpostInstanceTypesResponse (Prelude.Maybe [InstanceTypeItem])
getOutpostInstanceTypesResponse_instanceTypes = Lens.lens (\GetOutpostInstanceTypesResponse' {instanceTypes} -> instanceTypes) (\s@GetOutpostInstanceTypesResponse' {} a -> s {instanceTypes = a} :: GetOutpostInstanceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getOutpostInstanceTypesResponse_httpStatus :: Lens.Lens' GetOutpostInstanceTypesResponse Prelude.Int
getOutpostInstanceTypesResponse_httpStatus = Lens.lens (\GetOutpostInstanceTypesResponse' {httpStatus} -> httpStatus) (\s@GetOutpostInstanceTypesResponse' {} a -> s {httpStatus = a} :: GetOutpostInstanceTypesResponse)

instance
  Prelude.NFData
    GetOutpostInstanceTypesResponse
  where
  rnf GetOutpostInstanceTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf httpStatus
