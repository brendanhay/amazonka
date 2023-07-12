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
-- Module      : Amazonka.QuickSight.DescribeIpRestriction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a summary and status of IP rules.
module Amazonka.QuickSight.DescribeIpRestriction
  ( -- * Creating a Request
    DescribeIpRestriction (..),
    newDescribeIpRestriction,

    -- * Request Lenses
    describeIpRestriction_awsAccountId,

    -- * Destructuring the Response
    DescribeIpRestrictionResponse (..),
    newDescribeIpRestrictionResponse,

    -- * Response Lenses
    describeIpRestrictionResponse_awsAccountId,
    describeIpRestrictionResponse_enabled,
    describeIpRestrictionResponse_ipRestrictionRuleMap,
    describeIpRestrictionResponse_requestId,
    describeIpRestrictionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpRestriction' smart constructor.
data DescribeIpRestriction = DescribeIpRestriction'
  { -- | The ID of the Amazon Web Services account that contains the IP rules.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpRestriction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeIpRestriction_awsAccountId' - The ID of the Amazon Web Services account that contains the IP rules.
newDescribeIpRestriction ::
  -- | 'awsAccountId'
  Prelude.Text ->
  DescribeIpRestriction
newDescribeIpRestriction pAwsAccountId_ =
  DescribeIpRestriction'
    { awsAccountId =
        pAwsAccountId_
    }

-- | The ID of the Amazon Web Services account that contains the IP rules.
describeIpRestriction_awsAccountId :: Lens.Lens' DescribeIpRestriction Prelude.Text
describeIpRestriction_awsAccountId = Lens.lens (\DescribeIpRestriction' {awsAccountId} -> awsAccountId) (\s@DescribeIpRestriction' {} a -> s {awsAccountId = a} :: DescribeIpRestriction)

instance Core.AWSRequest DescribeIpRestriction where
  type
    AWSResponse DescribeIpRestriction =
      DescribeIpRestrictionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIpRestrictionResponse'
            Prelude.<$> (x Data..?> "AwsAccountId")
            Prelude.<*> (x Data..?> "Enabled")
            Prelude.<*> ( x
                            Data..?> "IpRestrictionRuleMap"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpRestriction where
  hashWithSalt _salt DescribeIpRestriction' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData DescribeIpRestriction where
  rnf DescribeIpRestriction' {..} =
    Prelude.rnf awsAccountId

instance Data.ToHeaders DescribeIpRestriction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeIpRestriction where
  toPath DescribeIpRestriction' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/ip-restriction"
      ]

instance Data.ToQuery DescribeIpRestriction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIpRestrictionResponse' smart constructor.
data DescribeIpRestrictionResponse = DescribeIpRestrictionResponse'
  { -- | The ID of the Amazon Web Services account that contains the IP rules.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies whether IP rules are turned on.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A map that describes the IP rules with CIDR range and description.
    ipRestrictionRuleMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpRestrictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeIpRestrictionResponse_awsAccountId' - The ID of the Amazon Web Services account that contains the IP rules.
--
-- 'enabled', 'describeIpRestrictionResponse_enabled' - A value that specifies whether IP rules are turned on.
--
-- 'ipRestrictionRuleMap', 'describeIpRestrictionResponse_ipRestrictionRuleMap' - A map that describes the IP rules with CIDR range and description.
--
-- 'requestId', 'describeIpRestrictionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeIpRestrictionResponse_status' - The HTTP status of the request.
newDescribeIpRestrictionResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeIpRestrictionResponse
newDescribeIpRestrictionResponse pStatus_ =
  DescribeIpRestrictionResponse'
    { awsAccountId =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      ipRestrictionRuleMap = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The ID of the Amazon Web Services account that contains the IP rules.
describeIpRestrictionResponse_awsAccountId :: Lens.Lens' DescribeIpRestrictionResponse (Prelude.Maybe Prelude.Text)
describeIpRestrictionResponse_awsAccountId = Lens.lens (\DescribeIpRestrictionResponse' {awsAccountId} -> awsAccountId) (\s@DescribeIpRestrictionResponse' {} a -> s {awsAccountId = a} :: DescribeIpRestrictionResponse)

-- | A value that specifies whether IP rules are turned on.
describeIpRestrictionResponse_enabled :: Lens.Lens' DescribeIpRestrictionResponse (Prelude.Maybe Prelude.Bool)
describeIpRestrictionResponse_enabled = Lens.lens (\DescribeIpRestrictionResponse' {enabled} -> enabled) (\s@DescribeIpRestrictionResponse' {} a -> s {enabled = a} :: DescribeIpRestrictionResponse)

-- | A map that describes the IP rules with CIDR range and description.
describeIpRestrictionResponse_ipRestrictionRuleMap :: Lens.Lens' DescribeIpRestrictionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeIpRestrictionResponse_ipRestrictionRuleMap = Lens.lens (\DescribeIpRestrictionResponse' {ipRestrictionRuleMap} -> ipRestrictionRuleMap) (\s@DescribeIpRestrictionResponse' {} a -> s {ipRestrictionRuleMap = a} :: DescribeIpRestrictionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeIpRestrictionResponse_requestId :: Lens.Lens' DescribeIpRestrictionResponse (Prelude.Maybe Prelude.Text)
describeIpRestrictionResponse_requestId = Lens.lens (\DescribeIpRestrictionResponse' {requestId} -> requestId) (\s@DescribeIpRestrictionResponse' {} a -> s {requestId = a} :: DescribeIpRestrictionResponse)

-- | The HTTP status of the request.
describeIpRestrictionResponse_status :: Lens.Lens' DescribeIpRestrictionResponse Prelude.Int
describeIpRestrictionResponse_status = Lens.lens (\DescribeIpRestrictionResponse' {status} -> status) (\s@DescribeIpRestrictionResponse' {} a -> s {status = a} :: DescribeIpRestrictionResponse)

instance Prelude.NFData DescribeIpRestrictionResponse where
  rnf DescribeIpRestrictionResponse' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ipRestrictionRuleMap
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
