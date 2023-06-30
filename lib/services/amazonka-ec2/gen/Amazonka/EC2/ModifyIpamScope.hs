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
-- Module      : Amazonka.EC2.ModifyIpamScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify an IPAM scope.
module Amazonka.EC2.ModifyIpamScope
  ( -- * Creating a Request
    ModifyIpamScope (..),
    newModifyIpamScope,

    -- * Request Lenses
    modifyIpamScope_description,
    modifyIpamScope_dryRun,
    modifyIpamScope_ipamScopeId,

    -- * Destructuring the Response
    ModifyIpamScopeResponse (..),
    newModifyIpamScopeResponse,

    -- * Response Lenses
    modifyIpamScopeResponse_ipamScope,
    modifyIpamScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyIpamScope' smart constructor.
data ModifyIpamScope = ModifyIpamScope'
  { -- | The description of the scope you want to modify.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the scope you want to modify.
    ipamScopeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'modifyIpamScope_description' - The description of the scope you want to modify.
--
-- 'dryRun', 'modifyIpamScope_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamScopeId', 'modifyIpamScope_ipamScopeId' - The ID of the scope you want to modify.
newModifyIpamScope ::
  -- | 'ipamScopeId'
  Prelude.Text ->
  ModifyIpamScope
newModifyIpamScope pIpamScopeId_ =
  ModifyIpamScope'
    { description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipamScopeId = pIpamScopeId_
    }

-- | The description of the scope you want to modify.
modifyIpamScope_description :: Lens.Lens' ModifyIpamScope (Prelude.Maybe Prelude.Text)
modifyIpamScope_description = Lens.lens (\ModifyIpamScope' {description} -> description) (\s@ModifyIpamScope' {} a -> s {description = a} :: ModifyIpamScope)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
modifyIpamScope_dryRun :: Lens.Lens' ModifyIpamScope (Prelude.Maybe Prelude.Bool)
modifyIpamScope_dryRun = Lens.lens (\ModifyIpamScope' {dryRun} -> dryRun) (\s@ModifyIpamScope' {} a -> s {dryRun = a} :: ModifyIpamScope)

-- | The ID of the scope you want to modify.
modifyIpamScope_ipamScopeId :: Lens.Lens' ModifyIpamScope Prelude.Text
modifyIpamScope_ipamScopeId = Lens.lens (\ModifyIpamScope' {ipamScopeId} -> ipamScopeId) (\s@ModifyIpamScope' {} a -> s {ipamScopeId = a} :: ModifyIpamScope)

instance Core.AWSRequest ModifyIpamScope where
  type
    AWSResponse ModifyIpamScope =
      ModifyIpamScopeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyIpamScopeResponse'
            Prelude.<$> (x Data..@? "ipamScope")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyIpamScope where
  hashWithSalt _salt ModifyIpamScope' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamScopeId

instance Prelude.NFData ModifyIpamScope where
  rnf ModifyIpamScope' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamScopeId

instance Data.ToHeaders ModifyIpamScope where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyIpamScope where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyIpamScope where
  toQuery ModifyIpamScope' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyIpamScope" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "IpamScopeId" Data.=: ipamScopeId
      ]

-- | /See:/ 'newModifyIpamScopeResponse' smart constructor.
data ModifyIpamScopeResponse = ModifyIpamScopeResponse'
  { -- | The results of the modification.
    ipamScope :: Prelude.Maybe IpamScope,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamScope', 'modifyIpamScopeResponse_ipamScope' - The results of the modification.
--
-- 'httpStatus', 'modifyIpamScopeResponse_httpStatus' - The response's http status code.
newModifyIpamScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyIpamScopeResponse
newModifyIpamScopeResponse pHttpStatus_ =
  ModifyIpamScopeResponse'
    { ipamScope =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The results of the modification.
modifyIpamScopeResponse_ipamScope :: Lens.Lens' ModifyIpamScopeResponse (Prelude.Maybe IpamScope)
modifyIpamScopeResponse_ipamScope = Lens.lens (\ModifyIpamScopeResponse' {ipamScope} -> ipamScope) (\s@ModifyIpamScopeResponse' {} a -> s {ipamScope = a} :: ModifyIpamScopeResponse)

-- | The response's http status code.
modifyIpamScopeResponse_httpStatus :: Lens.Lens' ModifyIpamScopeResponse Prelude.Int
modifyIpamScopeResponse_httpStatus = Lens.lens (\ModifyIpamScopeResponse' {httpStatus} -> httpStatus) (\s@ModifyIpamScopeResponse' {} a -> s {httpStatus = a} :: ModifyIpamScopeResponse)

instance Prelude.NFData ModifyIpamScopeResponse where
  rnf ModifyIpamScopeResponse' {..} =
    Prelude.rnf ipamScope
      `Prelude.seq` Prelude.rnf httpStatus
