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
-- Module      : Amazonka.Route53RecoveryReadiness.UpdateRecoveryGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Recovery Group.
module Amazonka.Route53RecoveryReadiness.UpdateRecoveryGroup
  ( -- * Creating a Request
    UpdateRecoveryGroup (..),
    newUpdateRecoveryGroup,

    -- * Request Lenses
    updateRecoveryGroup_recoveryGroupName,
    updateRecoveryGroup_cells,

    -- * Destructuring the Response
    UpdateRecoveryGroupResponse (..),
    newUpdateRecoveryGroupResponse,

    -- * Response Lenses
    updateRecoveryGroupResponse_cells,
    updateRecoveryGroupResponse_recoveryGroupName,
    updateRecoveryGroupResponse_recoveryGroupArn,
    updateRecoveryGroupResponse_tags,
    updateRecoveryGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | Parameters to update for the RecoveryGroup
--
-- /See:/ 'newUpdateRecoveryGroup' smart constructor.
data UpdateRecoveryGroup = UpdateRecoveryGroup'
  { -- | The RecoveryGroup to update
    recoveryGroupName :: Prelude.Text,
    -- | A list of Cell arns, completely replaces previous list
    cells :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecoveryGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryGroupName', 'updateRecoveryGroup_recoveryGroupName' - The RecoveryGroup to update
--
-- 'cells', 'updateRecoveryGroup_cells' - A list of Cell arns, completely replaces previous list
newUpdateRecoveryGroup ::
  -- | 'recoveryGroupName'
  Prelude.Text ->
  UpdateRecoveryGroup
newUpdateRecoveryGroup pRecoveryGroupName_ =
  UpdateRecoveryGroup'
    { recoveryGroupName =
        pRecoveryGroupName_,
      cells = Prelude.mempty
    }

-- | The RecoveryGroup to update
updateRecoveryGroup_recoveryGroupName :: Lens.Lens' UpdateRecoveryGroup Prelude.Text
updateRecoveryGroup_recoveryGroupName = Lens.lens (\UpdateRecoveryGroup' {recoveryGroupName} -> recoveryGroupName) (\s@UpdateRecoveryGroup' {} a -> s {recoveryGroupName = a} :: UpdateRecoveryGroup)

-- | A list of Cell arns, completely replaces previous list
updateRecoveryGroup_cells :: Lens.Lens' UpdateRecoveryGroup [Prelude.Text]
updateRecoveryGroup_cells = Lens.lens (\UpdateRecoveryGroup' {cells} -> cells) (\s@UpdateRecoveryGroup' {} a -> s {cells = a} :: UpdateRecoveryGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRecoveryGroup where
  type
    AWSResponse UpdateRecoveryGroup =
      UpdateRecoveryGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecoveryGroupResponse'
            Prelude.<$> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "recoveryGroupName")
            Prelude.<*> (x Core..?> "recoveryGroupArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRecoveryGroup

instance Prelude.NFData UpdateRecoveryGroup

instance Core.ToHeaders UpdateRecoveryGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRecoveryGroup where
  toJSON UpdateRecoveryGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("cells" Core..= cells)]
      )

instance Core.ToPath UpdateRecoveryGroup where
  toPath UpdateRecoveryGroup' {..} =
    Prelude.mconcat
      ["/recoverygroups/", Core.toBS recoveryGroupName]

instance Core.ToQuery UpdateRecoveryGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRecoveryGroupResponse' smart constructor.
data UpdateRecoveryGroupResponse = UpdateRecoveryGroupResponse'
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
-- Create a value of 'UpdateRecoveryGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'updateRecoveryGroupResponse_cells' - A list of Cell arns
--
-- 'recoveryGroupName', 'updateRecoveryGroupResponse_recoveryGroupName' - The name of the RecoveryGroup
--
-- 'recoveryGroupArn', 'updateRecoveryGroupResponse_recoveryGroupArn' - The arn for the RecoveryGroup
--
-- 'tags', 'updateRecoveryGroupResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'updateRecoveryGroupResponse_httpStatus' - The response's http status code.
newUpdateRecoveryGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRecoveryGroupResponse
newUpdateRecoveryGroupResponse pHttpStatus_ =
  UpdateRecoveryGroupResponse'
    { cells =
        Prelude.Nothing,
      recoveryGroupName = Prelude.Nothing,
      recoveryGroupArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Cell arns
updateRecoveryGroupResponse_cells :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe [Prelude.Text])
updateRecoveryGroupResponse_cells = Lens.lens (\UpdateRecoveryGroupResponse' {cells} -> cells) (\s@UpdateRecoveryGroupResponse' {} a -> s {cells = a} :: UpdateRecoveryGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the RecoveryGroup
updateRecoveryGroupResponse_recoveryGroupName :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe Prelude.Text)
updateRecoveryGroupResponse_recoveryGroupName = Lens.lens (\UpdateRecoveryGroupResponse' {recoveryGroupName} -> recoveryGroupName) (\s@UpdateRecoveryGroupResponse' {} a -> s {recoveryGroupName = a} :: UpdateRecoveryGroupResponse)

-- | The arn for the RecoveryGroup
updateRecoveryGroupResponse_recoveryGroupArn :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe Prelude.Text)
updateRecoveryGroupResponse_recoveryGroupArn = Lens.lens (\UpdateRecoveryGroupResponse' {recoveryGroupArn} -> recoveryGroupArn) (\s@UpdateRecoveryGroupResponse' {} a -> s {recoveryGroupArn = a} :: UpdateRecoveryGroupResponse)

-- | Undocumented member.
updateRecoveryGroupResponse_tags :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRecoveryGroupResponse_tags = Lens.lens (\UpdateRecoveryGroupResponse' {tags} -> tags) (\s@UpdateRecoveryGroupResponse' {} a -> s {tags = a} :: UpdateRecoveryGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateRecoveryGroupResponse_httpStatus :: Lens.Lens' UpdateRecoveryGroupResponse Prelude.Int
updateRecoveryGroupResponse_httpStatus = Lens.lens (\UpdateRecoveryGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRecoveryGroupResponse' {} a -> s {httpStatus = a} :: UpdateRecoveryGroupResponse)

instance Prelude.NFData UpdateRecoveryGroupResponse
