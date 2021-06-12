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
-- Module      : Network.AWS.RDS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the attributes for a customer account. The attributes
-- include Amazon RDS quotas for the account, such as the number of DB
-- instances allowed. The description for a quota includes the quota name,
-- current usage toward that quota, and the quota\'s maximum value.
--
-- This command doesn\'t take any parameters.
module Network.AWS.RDS.DescribeAccountAttributes
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Core.<$> ( x Core..@? "AccountQuotas" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "AccountQuota")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAccountAttributes

instance Core.NFData DescribeAccountAttributes

instance Core.ToHeaders DescribeAccountAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAccountAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAccountAttributes where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeAccountAttributes" :: Core.ByteString),
            "Version" Core.=: ("2014-10-31" :: Core.ByteString)
          ]
      )

-- | Data returned by the __DescribeAccountAttributes__ action.
--
-- /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | A list of @AccountQuota@ objects. Within this list, each quota has a
    -- name, a count of usage toward the quota maximum, and a maximum value for
    -- the quota.
    accountQuotas :: Core.Maybe [AccountQuota],
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
-- 'accountQuotas', 'describeAccountAttributesResponse_accountQuotas' - A list of @AccountQuota@ objects. Within this list, each quota has a
-- name, a count of usage toward the quota maximum, and a maximum value for
-- the quota.
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
      httpStatus = pHttpStatus_
    }

-- | A list of @AccountQuota@ objects. Within this list, each quota has a
-- name, a count of usage toward the quota maximum, and a maximum value for
-- the quota.
describeAccountAttributesResponse_accountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [AccountQuota])
describeAccountAttributesResponse_accountQuotas = Lens.lens (\DescribeAccountAttributesResponse' {accountQuotas} -> accountQuotas) (\s@DescribeAccountAttributesResponse' {} a -> s {accountQuotas = a} :: DescribeAccountAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Core.NFData
    DescribeAccountAttributesResponse
