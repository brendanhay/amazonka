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
-- Module      : Network.AWS.Shield.DescribeProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specification for the specified protection group.
module Network.AWS.Shield.DescribeProtectionGroup
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ProtectionGroup")
      )

instance Prelude.Hashable DescribeProtectionGroup

instance Prelude.NFData DescribeProtectionGroup

instance Core.ToHeaders DescribeProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DescribeProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProtectionGroup where
  toJSON DescribeProtectionGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProtectionGroupId" Core..= protectionGroupId)
          ]
      )

instance Core.ToPath DescribeProtectionGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProtectionGroupResponse' smart constructor.
data DescribeProtectionGroupResponse = DescribeProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A grouping of protected resources that you and AWS Shield Advanced can
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
-- 'protectionGroup', 'describeProtectionGroupResponse_protectionGroup' - A grouping of protected resources that you and AWS Shield Advanced can
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

-- | A grouping of protected resources that you and AWS Shield Advanced can
-- monitor as a collective. This resource grouping improves the accuracy of
-- detection and reduces false positives.
describeProtectionGroupResponse_protectionGroup :: Lens.Lens' DescribeProtectionGroupResponse ProtectionGroup
describeProtectionGroupResponse_protectionGroup = Lens.lens (\DescribeProtectionGroupResponse' {protectionGroup} -> protectionGroup) (\s@DescribeProtectionGroupResponse' {} a -> s {protectionGroup = a} :: DescribeProtectionGroupResponse)

instance
  Prelude.NFData
    DescribeProtectionGroupResponse
