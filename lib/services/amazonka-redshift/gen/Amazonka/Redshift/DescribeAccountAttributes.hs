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
-- Module      : Amazonka.Redshift.DescribeAccountAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attributes attached to an account
module Amazonka.Redshift.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Request Lenses
    describeAccountAttributes_attributeNames,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  { -- | A list of attribute names.
    attributeNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'describeAccountAttributes_attributeNames' - A list of attribute names.
newDescribeAccountAttributes ::
  DescribeAccountAttributes
newDescribeAccountAttributes =
  DescribeAccountAttributes'
    { attributeNames =
        Prelude.Nothing
    }

-- | A list of attribute names.
describeAccountAttributes_attributeNames :: Lens.Lens' DescribeAccountAttributes (Prelude.Maybe [Prelude.Text])
describeAccountAttributes_attributeNames = Lens.lens (\DescribeAccountAttributes' {attributeNames} -> attributeNames) (\s@DescribeAccountAttributes' {} a -> s {attributeNames = a} :: DescribeAccountAttributes) Prelude.. Lens.mapping Lens.coerced

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
                            Data..@? "AccountAttributes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "AccountAttribute")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountAttributes where
  hashWithSalt _salt DescribeAccountAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributeNames

instance Prelude.NFData DescribeAccountAttributes where
  rnf DescribeAccountAttributes' {..} =
    Prelude.rnf attributeNames

instance Data.ToHeaders DescribeAccountAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccountAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountAttributes where
  toQuery DescribeAccountAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAccountAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AttributeNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "AttributeName"
                Prelude.<$> attributeNames
            )
      ]

-- | /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | A list of attributes assigned to an account.
    accountAttributes :: Prelude.Maybe [AccountAttribute],
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
-- 'accountAttributes', 'describeAccountAttributesResponse_accountAttributes' - A list of attributes assigned to an account.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { accountAttributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of attributes assigned to an account.
describeAccountAttributesResponse_accountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Prelude.Maybe [AccountAttribute])
describeAccountAttributesResponse_accountAttributes = Lens.lens (\DescribeAccountAttributesResponse' {accountAttributes} -> accountAttributes) (\s@DescribeAccountAttributesResponse' {} a -> s {accountAttributes = a} :: DescribeAccountAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Prelude.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Prelude.NFData
    DescribeAccountAttributesResponse
  where
  rnf DescribeAccountAttributesResponse' {..} =
    Prelude.rnf accountAttributes `Prelude.seq`
      Prelude.rnf httpStatus
