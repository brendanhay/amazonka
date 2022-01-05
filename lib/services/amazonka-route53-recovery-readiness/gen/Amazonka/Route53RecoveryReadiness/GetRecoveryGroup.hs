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
-- Module      : Amazonka.Route53RecoveryReadiness.GetRecoveryGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a Recovery Group.
module Amazonka.Route53RecoveryReadiness.GetRecoveryGroup
  ( -- * Creating a Request
    GetRecoveryGroup (..),
    newGetRecoveryGroup,

    -- * Request Lenses
    getRecoveryGroup_recoveryGroupName,

    -- * Destructuring the Response
    GetRecoveryGroupResponse (..),
    newGetRecoveryGroupResponse,

    -- * Response Lenses
    getRecoveryGroupResponse_cells,
    getRecoveryGroupResponse_recoveryGroupName,
    getRecoveryGroupResponse_recoveryGroupArn,
    getRecoveryGroupResponse_tags,
    getRecoveryGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetRecoveryGroup' smart constructor.
data GetRecoveryGroup = GetRecoveryGroup'
  { -- | The RecoveryGroup to get
    recoveryGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecoveryGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryGroupName', 'getRecoveryGroup_recoveryGroupName' - The RecoveryGroup to get
newGetRecoveryGroup ::
  -- | 'recoveryGroupName'
  Prelude.Text ->
  GetRecoveryGroup
newGetRecoveryGroup pRecoveryGroupName_ =
  GetRecoveryGroup'
    { recoveryGroupName =
        pRecoveryGroupName_
    }

-- | The RecoveryGroup to get
getRecoveryGroup_recoveryGroupName :: Lens.Lens' GetRecoveryGroup Prelude.Text
getRecoveryGroup_recoveryGroupName = Lens.lens (\GetRecoveryGroup' {recoveryGroupName} -> recoveryGroupName) (\s@GetRecoveryGroup' {} a -> s {recoveryGroupName = a} :: GetRecoveryGroup)

instance Core.AWSRequest GetRecoveryGroup where
  type
    AWSResponse GetRecoveryGroup =
      GetRecoveryGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecoveryGroupResponse'
            Prelude.<$> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "recoveryGroupName")
            Prelude.<*> (x Core..?> "recoveryGroupArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRecoveryGroup where
  hashWithSalt _salt GetRecoveryGroup' {..} =
    _salt `Prelude.hashWithSalt` recoveryGroupName

instance Prelude.NFData GetRecoveryGroup where
  rnf GetRecoveryGroup' {..} =
    Prelude.rnf recoveryGroupName

instance Core.ToHeaders GetRecoveryGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRecoveryGroup where
  toPath GetRecoveryGroup' {..} =
    Prelude.mconcat
      ["/recoverygroups/", Core.toBS recoveryGroupName]

instance Core.ToQuery GetRecoveryGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecoveryGroupResponse' smart constructor.
data GetRecoveryGroupResponse = GetRecoveryGroupResponse'
  { -- | A list of Cell arns
    cells :: Prelude.Maybe [Prelude.Text],
    -- | The name of the RecoveryGroup
    recoveryGroupName :: Prelude.Maybe Prelude.Text,
    -- | The arn for the RecoveryGroup
    recoveryGroupArn :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecoveryGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'getRecoveryGroupResponse_cells' - A list of Cell arns
--
-- 'recoveryGroupName', 'getRecoveryGroupResponse_recoveryGroupName' - The name of the RecoveryGroup
--
-- 'recoveryGroupArn', 'getRecoveryGroupResponse_recoveryGroupArn' - The arn for the RecoveryGroup
--
-- 'tags', 'getRecoveryGroupResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'getRecoveryGroupResponse_httpStatus' - The response's http status code.
newGetRecoveryGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecoveryGroupResponse
newGetRecoveryGroupResponse pHttpStatus_ =
  GetRecoveryGroupResponse'
    { cells = Prelude.Nothing,
      recoveryGroupName = Prelude.Nothing,
      recoveryGroupArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Cell arns
getRecoveryGroupResponse_cells :: Lens.Lens' GetRecoveryGroupResponse (Prelude.Maybe [Prelude.Text])
getRecoveryGroupResponse_cells = Lens.lens (\GetRecoveryGroupResponse' {cells} -> cells) (\s@GetRecoveryGroupResponse' {} a -> s {cells = a} :: GetRecoveryGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the RecoveryGroup
getRecoveryGroupResponse_recoveryGroupName :: Lens.Lens' GetRecoveryGroupResponse (Prelude.Maybe Prelude.Text)
getRecoveryGroupResponse_recoveryGroupName = Lens.lens (\GetRecoveryGroupResponse' {recoveryGroupName} -> recoveryGroupName) (\s@GetRecoveryGroupResponse' {} a -> s {recoveryGroupName = a} :: GetRecoveryGroupResponse)

-- | The arn for the RecoveryGroup
getRecoveryGroupResponse_recoveryGroupArn :: Lens.Lens' GetRecoveryGroupResponse (Prelude.Maybe Prelude.Text)
getRecoveryGroupResponse_recoveryGroupArn = Lens.lens (\GetRecoveryGroupResponse' {recoveryGroupArn} -> recoveryGroupArn) (\s@GetRecoveryGroupResponse' {} a -> s {recoveryGroupArn = a} :: GetRecoveryGroupResponse)

-- | Undocumented member.
getRecoveryGroupResponse_tags :: Lens.Lens' GetRecoveryGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRecoveryGroupResponse_tags = Lens.lens (\GetRecoveryGroupResponse' {tags} -> tags) (\s@GetRecoveryGroupResponse' {} a -> s {tags = a} :: GetRecoveryGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecoveryGroupResponse_httpStatus :: Lens.Lens' GetRecoveryGroupResponse Prelude.Int
getRecoveryGroupResponse_httpStatus = Lens.lens (\GetRecoveryGroupResponse' {httpStatus} -> httpStatus) (\s@GetRecoveryGroupResponse' {} a -> s {httpStatus = a} :: GetRecoveryGroupResponse)

instance Prelude.NFData GetRecoveryGroupResponse where
  rnf GetRecoveryGroupResponse' {..} =
    Prelude.rnf cells
      `Prelude.seq` Prelude.rnf recoveryGroupName
      `Prelude.seq` Prelude.rnf recoveryGroupArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
