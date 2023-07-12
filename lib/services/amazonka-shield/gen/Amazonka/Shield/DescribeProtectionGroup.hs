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
-- Module      : Amazonka.Shield.DescribeProtectionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specification for the specified protection group.
module Amazonka.Shield.DescribeProtectionGroup
  ( -- * Creating a Request
    DescribeProtectionGroup (..),
    newDescribeProtectionGroup,

    -- * Request Lenses
    describeProtectionGroup_protectionGroupId,

    -- * Destructuring the Response
    DescribeProtectionGroupResponse (..),
    newDescribeProtectionGroupResponse,

    -- * Response Lenses
    describeProtectionGroupResponse_httpStatus,
    describeProtectionGroupResponse_protectionGroup,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDescribeProtectionGroup' smart constructor.
data DescribeProtectionGroup = DescribeProtectionGroup'
  { -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionGroupId', 'describeProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
newDescribeProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  DescribeProtectionGroup
newDescribeProtectionGroup pProtectionGroupId_ =
  DescribeProtectionGroup'
    { protectionGroupId =
        pProtectionGroupId_
    }

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
describeProtectionGroup_protectionGroupId :: Lens.Lens' DescribeProtectionGroup Prelude.Text
describeProtectionGroup_protectionGroupId = Lens.lens (\DescribeProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@DescribeProtectionGroup' {} a -> s {protectionGroupId = a} :: DescribeProtectionGroup)

instance Core.AWSRequest DescribeProtectionGroup where
  type
    AWSResponse DescribeProtectionGroup =
      DescribeProtectionGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProtectionGroup")
      )

instance Prelude.Hashable DescribeProtectionGroup where
  hashWithSalt _salt DescribeProtectionGroup' {..} =
    _salt `Prelude.hashWithSalt` protectionGroupId

instance Prelude.NFData DescribeProtectionGroup where
  rnf DescribeProtectionGroup' {..} =
    Prelude.rnf protectionGroupId

instance Data.ToHeaders DescribeProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DescribeProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProtectionGroup where
  toJSON DescribeProtectionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProtectionGroupId" Data..= protectionGroupId)
          ]
      )

instance Data.ToPath DescribeProtectionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProtectionGroupResponse' smart constructor.
data DescribeProtectionGroupResponse = DescribeProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A grouping of protected resources that you and Shield Advanced can
    -- monitor as a collective. This resource grouping improves the accuracy of
    -- detection and reduces false positives.
    protectionGroup :: ProtectionGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProtectionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeProtectionGroupResponse_httpStatus' - The response's http status code.
--
-- 'protectionGroup', 'describeProtectionGroupResponse_protectionGroup' - A grouping of protected resources that you and Shield Advanced can
-- monitor as a collective. This resource grouping improves the accuracy of
-- detection and reduces false positives.
newDescribeProtectionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'protectionGroup'
  ProtectionGroup ->
  DescribeProtectionGroupResponse
newDescribeProtectionGroupResponse
  pHttpStatus_
  pProtectionGroup_ =
    DescribeProtectionGroupResponse'
      { httpStatus =
          pHttpStatus_,
        protectionGroup = pProtectionGroup_
      }

-- | The response's http status code.
describeProtectionGroupResponse_httpStatus :: Lens.Lens' DescribeProtectionGroupResponse Prelude.Int
describeProtectionGroupResponse_httpStatus = Lens.lens (\DescribeProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeProtectionGroupResponse' {} a -> s {httpStatus = a} :: DescribeProtectionGroupResponse)

-- | A grouping of protected resources that you and Shield Advanced can
-- monitor as a collective. This resource grouping improves the accuracy of
-- detection and reduces false positives.
describeProtectionGroupResponse_protectionGroup :: Lens.Lens' DescribeProtectionGroupResponse ProtectionGroup
describeProtectionGroupResponse_protectionGroup = Lens.lens (\DescribeProtectionGroupResponse' {protectionGroup} -> protectionGroup) (\s@DescribeProtectionGroupResponse' {} a -> s {protectionGroup = a} :: DescribeProtectionGroupResponse)

instance
  Prelude.NFData
    DescribeProtectionGroupResponse
  where
  rnf DescribeProtectionGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf protectionGroup
