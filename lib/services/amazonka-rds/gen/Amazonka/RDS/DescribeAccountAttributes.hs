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
-- Module      : Amazonka.RDS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the attributes for a customer account. The attributes
-- include Amazon RDS quotas for the account, such as the number of DB
-- instances allowed. The description for a quota includes the quota name,
-- current usage toward that quota, and the quota\'s maximum value.
--
-- This command doesn\'t take any parameters.
module Amazonka.RDS.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountAttributes ::
  DescribeAccountAttributes
newDescribeAccountAttributes =
  DescribeAccountAttributes'

instance Core.AWSRequest DescribeAccountAttributes where
  type
    AWSResponse DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Prelude.<$> ( x
                            Data..@? "AccountQuotas"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "AccountQuota")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountAttributes where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeAccountAttributes where
  rnf _ = ()

instance Data.ToHeaders DescribeAccountAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccountAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountAttributes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("DescribeAccountAttributes" :: Prelude.ByteString),
            "Version"
              Data.=: ("2014-10-31" :: Prelude.ByteString)
          ]
      )

-- | Data returned by the __DescribeAccountAttributes__ action.
--
-- /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | A list of @AccountQuota@ objects. Within this list, each quota has a
    -- name, a count of usage toward the quota maximum, and a maximum value for
    -- the quota.
    accountQuotas :: Prelude.Maybe [AccountQuota],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountQuotas', 'describeAccountAttributesResponse_accountQuotas' - A list of @AccountQuota@ objects. Within this list, each quota has a
-- name, a count of usage toward the quota maximum, and a maximum value for
-- the quota.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { accountQuotas =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @AccountQuota@ objects. Within this list, each quota has a
-- name, a count of usage toward the quota maximum, and a maximum value for
-- the quota.
describeAccountAttributesResponse_accountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Prelude.Maybe [AccountQuota])
describeAccountAttributesResponse_accountQuotas = Lens.lens (\DescribeAccountAttributesResponse' {accountQuotas} -> accountQuotas) (\s@DescribeAccountAttributesResponse' {} a -> s {accountQuotas = a} :: DescribeAccountAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Prelude.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Prelude.NFData
    DescribeAccountAttributesResponse
  where
  rnf DescribeAccountAttributesResponse' {..} =
    Prelude.rnf accountQuotas
      `Prelude.seq` Prelude.rnf httpStatus
