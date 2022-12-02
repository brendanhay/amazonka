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
-- Module      : Amazonka.DMS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the DMS attributes for a customer account. These attributes
-- include DMS quotas for the account and a unique account identifier in a
-- particular DMS region. DMS quotas include a list of resource quotas
-- supported by the account, such as the number of replication instances
-- allowed. The description for each resource quota, includes the quota
-- name, current usage toward that quota, and the quota\'s maximum value.
-- DMS uses the unique account identifier to name each artifact used by DMS
-- in the given region.
--
-- This command does not take any parameters.
module Amazonka.DMS.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_uniqueAccountIdentifier,
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
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
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Prelude.<$> (x Data..?> "UniqueAccountIdentifier")
            Prelude.<*> (x Data..?> "AccountQuotas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountAttributes where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeAccountAttributes where
  rnf _ = ()

instance Data.ToHeaders DescribeAccountAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeAccountAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccountAttributes where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeAccountAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountAttributes where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | A unique DMS identifier for an account in a particular Amazon Web
    -- Services Region. The value of this identifier has the following format:
    -- @c99999999999@. DMS uses this identifier to name artifacts. For example,
    -- DMS uses this identifier to name the default Amazon S3 bucket for
    -- storing task assessment reports in a given Amazon Web Services Region.
    -- The format of this S3 bucket name is the following:
    -- @dms-AccountNumber-UniqueAccountIdentifier.@ Here is an example name for
    -- this default S3 bucket: @dms-111122223333-c44445555666@.
    --
    -- DMS supports the @UniqueAccountIdentifier@ parameter in versions 3.1.4
    -- and later.
    uniqueAccountIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Account quota information.
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
-- 'uniqueAccountIdentifier', 'describeAccountAttributesResponse_uniqueAccountIdentifier' - A unique DMS identifier for an account in a particular Amazon Web
-- Services Region. The value of this identifier has the following format:
-- @c99999999999@. DMS uses this identifier to name artifacts. For example,
-- DMS uses this identifier to name the default Amazon S3 bucket for
-- storing task assessment reports in a given Amazon Web Services Region.
-- The format of this S3 bucket name is the following:
-- @dms-AccountNumber-UniqueAccountIdentifier.@ Here is an example name for
-- this default S3 bucket: @dms-111122223333-c44445555666@.
--
-- DMS supports the @UniqueAccountIdentifier@ parameter in versions 3.1.4
-- and later.
--
-- 'accountQuotas', 'describeAccountAttributesResponse_accountQuotas' - Account quota information.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { uniqueAccountIdentifier =
        Prelude.Nothing,
      accountQuotas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique DMS identifier for an account in a particular Amazon Web
-- Services Region. The value of this identifier has the following format:
-- @c99999999999@. DMS uses this identifier to name artifacts. For example,
-- DMS uses this identifier to name the default Amazon S3 bucket for
-- storing task assessment reports in a given Amazon Web Services Region.
-- The format of this S3 bucket name is the following:
-- @dms-AccountNumber-UniqueAccountIdentifier.@ Here is an example name for
-- this default S3 bucket: @dms-111122223333-c44445555666@.
--
-- DMS supports the @UniqueAccountIdentifier@ parameter in versions 3.1.4
-- and later.
describeAccountAttributesResponse_uniqueAccountIdentifier :: Lens.Lens' DescribeAccountAttributesResponse (Prelude.Maybe Prelude.Text)
describeAccountAttributesResponse_uniqueAccountIdentifier = Lens.lens (\DescribeAccountAttributesResponse' {uniqueAccountIdentifier} -> uniqueAccountIdentifier) (\s@DescribeAccountAttributesResponse' {} a -> s {uniqueAccountIdentifier = a} :: DescribeAccountAttributesResponse)

-- | Account quota information.
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
    Prelude.rnf uniqueAccountIdentifier
      `Prelude.seq` Prelude.rnf accountQuotas
      `Prelude.seq` Prelude.rnf httpStatus
