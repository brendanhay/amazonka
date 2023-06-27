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
-- Module      : Amazonka.CloudFormation.DescribeOrganizationsAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the account\'s @OrganizationAccess@ status.
-- This API can be called either by the management account or the delegated
-- administrator by using the @CallAs@ parameter. This API can also be
-- called without the @CallAs@ parameter by the management account.
module Amazonka.CloudFormation.DescribeOrganizationsAccess
  ( -- * Creating a Request
    DescribeOrganizationsAccess (..),
    newDescribeOrganizationsAccess,

    -- * Request Lenses
    describeOrganizationsAccess_callAs,

    -- * Destructuring the Response
    DescribeOrganizationsAccessResponse (..),
    newDescribeOrganizationsAccessResponse,

    -- * Response Lenses
    describeOrganizationsAccessResponse_status,
    describeOrganizationsAccessResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationsAccess' smart constructor.
data DescribeOrganizationsAccess = DescribeOrganizationsAccess'
  { -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationsAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'describeOrganizationsAccess_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
newDescribeOrganizationsAccess ::
  DescribeOrganizationsAccess
newDescribeOrganizationsAccess =
  DescribeOrganizationsAccess'
    { callAs =
        Prelude.Nothing
    }

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
describeOrganizationsAccess_callAs :: Lens.Lens' DescribeOrganizationsAccess (Prelude.Maybe CallAs)
describeOrganizationsAccess_callAs = Lens.lens (\DescribeOrganizationsAccess' {callAs} -> callAs) (\s@DescribeOrganizationsAccess' {} a -> s {callAs = a} :: DescribeOrganizationsAccess)

instance Core.AWSRequest DescribeOrganizationsAccess where
  type
    AWSResponse DescribeOrganizationsAccess =
      DescribeOrganizationsAccessResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeOrganizationsAccessResult"
      ( \s h x ->
          DescribeOrganizationsAccessResponse'
            Prelude.<$> (x Data..@? "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOrganizationsAccess where
  hashWithSalt _salt DescribeOrganizationsAccess' {..} =
    _salt `Prelude.hashWithSalt` callAs

instance Prelude.NFData DescribeOrganizationsAccess where
  rnf DescribeOrganizationsAccess' {..} =
    Prelude.rnf callAs

instance Data.ToHeaders DescribeOrganizationsAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeOrganizationsAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOrganizationsAccess where
  toQuery DescribeOrganizationsAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeOrganizationsAccess" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Data.=: callAs
      ]

-- | /See:/ 'newDescribeOrganizationsAccessResponse' smart constructor.
data DescribeOrganizationsAccessResponse = DescribeOrganizationsAccessResponse'
  { -- | Presents the status of the @OrganizationAccess@.
    status :: Prelude.Maybe OrganizationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationsAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeOrganizationsAccessResponse_status' - Presents the status of the @OrganizationAccess@.
--
-- 'httpStatus', 'describeOrganizationsAccessResponse_httpStatus' - The response's http status code.
newDescribeOrganizationsAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationsAccessResponse
newDescribeOrganizationsAccessResponse pHttpStatus_ =
  DescribeOrganizationsAccessResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Presents the status of the @OrganizationAccess@.
describeOrganizationsAccessResponse_status :: Lens.Lens' DescribeOrganizationsAccessResponse (Prelude.Maybe OrganizationStatus)
describeOrganizationsAccessResponse_status = Lens.lens (\DescribeOrganizationsAccessResponse' {status} -> status) (\s@DescribeOrganizationsAccessResponse' {} a -> s {status = a} :: DescribeOrganizationsAccessResponse)

-- | The response's http status code.
describeOrganizationsAccessResponse_httpStatus :: Lens.Lens' DescribeOrganizationsAccessResponse Prelude.Int
describeOrganizationsAccessResponse_httpStatus = Lens.lens (\DescribeOrganizationsAccessResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationsAccessResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationsAccessResponse)

instance
  Prelude.NFData
    DescribeOrganizationsAccessResponse
  where
  rnf DescribeOrganizationsAccessResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
