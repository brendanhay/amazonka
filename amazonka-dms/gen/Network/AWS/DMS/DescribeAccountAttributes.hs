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
-- Module      : Network.AWS.DMS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the AWS DMS attributes for a customer account. These
-- attributes include AWS DMS quotas for the account and a unique account
-- identifier in a particular DMS region. DMS quotas include a list of
-- resource quotas supported by the account, such as the number of
-- replication instances allowed. The description for each resource quota,
-- includes the quota name, current usage toward that quota, and the
-- quota\'s maximum value. DMS uses the unique account identifier to name
-- each artifact used by DMS in the given region.
--
-- This command does not take any parameters.
module Network.AWS.DMS.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_accountQuotas,
    describeAccountAttributesResponse_uniqueAccountIdentifier,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Core.<$> (x Core..?> "AccountQuotas" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "UniqueAccountIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAccountAttributes

instance Core.NFData DescribeAccountAttributes

instance Core.ToHeaders DescribeAccountAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeAccountAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAccountAttributes where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeAccountAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAccountAttributes where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | Account quota information.
    accountQuotas :: Core.Maybe [AccountQuota],
    -- | A unique AWS DMS identifier for an account in a particular AWS Region.
    -- The value of this identifier has the following format: @c99999999999@.
    -- DMS uses this identifier to name artifacts. For example, DMS uses this
    -- identifier to name the default Amazon S3 bucket for storing task
    -- assessment reports in a given AWS Region. The format of this S3 bucket
    -- name is the following: @dms-AccountNumber-UniqueAccountIdentifier.@ Here
    -- is an example name for this default S3 bucket:
    -- @dms-111122223333-c44445555666@.
    --
    -- AWS DMS supports the @UniqueAccountIdentifier@ parameter in versions
    -- 3.1.4 and later.
    uniqueAccountIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAccountAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountQuotas', 'describeAccountAttributesResponse_accountQuotas' - Account quota information.
--
-- 'uniqueAccountIdentifier', 'describeAccountAttributesResponse_uniqueAccountIdentifier' - A unique AWS DMS identifier for an account in a particular AWS Region.
-- The value of this identifier has the following format: @c99999999999@.
-- DMS uses this identifier to name artifacts. For example, DMS uses this
-- identifier to name the default Amazon S3 bucket for storing task
-- assessment reports in a given AWS Region. The format of this S3 bucket
-- name is the following: @dms-AccountNumber-UniqueAccountIdentifier.@ Here
-- is an example name for this default S3 bucket:
-- @dms-111122223333-c44445555666@.
--
-- AWS DMS supports the @UniqueAccountIdentifier@ parameter in versions
-- 3.1.4 and later.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { accountQuotas =
        Core.Nothing,
      uniqueAccountIdentifier = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Account quota information.
describeAccountAttributesResponse_accountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [AccountQuota])
describeAccountAttributesResponse_accountQuotas = Lens.lens (\DescribeAccountAttributesResponse' {accountQuotas} -> accountQuotas) (\s@DescribeAccountAttributesResponse' {} a -> s {accountQuotas = a} :: DescribeAccountAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | A unique AWS DMS identifier for an account in a particular AWS Region.
-- The value of this identifier has the following format: @c99999999999@.
-- DMS uses this identifier to name artifacts. For example, DMS uses this
-- identifier to name the default Amazon S3 bucket for storing task
-- assessment reports in a given AWS Region. The format of this S3 bucket
-- name is the following: @dms-AccountNumber-UniqueAccountIdentifier.@ Here
-- is an example name for this default S3 bucket:
-- @dms-111122223333-c44445555666@.
--
-- AWS DMS supports the @UniqueAccountIdentifier@ parameter in versions
-- 3.1.4 and later.
describeAccountAttributesResponse_uniqueAccountIdentifier :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe Core.Text)
describeAccountAttributesResponse_uniqueAccountIdentifier = Lens.lens (\DescribeAccountAttributesResponse' {uniqueAccountIdentifier} -> uniqueAccountIdentifier) (\s@DescribeAccountAttributesResponse' {} a -> s {uniqueAccountIdentifier = a} :: DescribeAccountAttributesResponse)

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Core.NFData
    DescribeAccountAttributesResponse
