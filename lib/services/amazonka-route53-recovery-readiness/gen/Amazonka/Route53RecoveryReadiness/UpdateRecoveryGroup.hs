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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a recovery group.
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
    updateRecoveryGroupResponse_tags,
    updateRecoveryGroupResponse_recoveryGroupArn,
    updateRecoveryGroupResponse_recoveryGroupName,
    updateRecoveryGroupResponse_cells,
    updateRecoveryGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | Name of a recovery group.
--
-- /See:/ 'newUpdateRecoveryGroup' smart constructor.
data UpdateRecoveryGroup = UpdateRecoveryGroup'
  { -- | The name of a recovery group.
    recoveryGroupName :: Prelude.Text,
    -- | A list of cell Amazon Resource Names (ARNs). This list completely
    -- replaces the previous list.
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
-- 'recoveryGroupName', 'updateRecoveryGroup_recoveryGroupName' - The name of a recovery group.
--
-- 'cells', 'updateRecoveryGroup_cells' - A list of cell Amazon Resource Names (ARNs). This list completely
-- replaces the previous list.
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

-- | The name of a recovery group.
updateRecoveryGroup_recoveryGroupName :: Lens.Lens' UpdateRecoveryGroup Prelude.Text
updateRecoveryGroup_recoveryGroupName = Lens.lens (\UpdateRecoveryGroup' {recoveryGroupName} -> recoveryGroupName) (\s@UpdateRecoveryGroup' {} a -> s {recoveryGroupName = a} :: UpdateRecoveryGroup)

-- | A list of cell Amazon Resource Names (ARNs). This list completely
-- replaces the previous list.
updateRecoveryGroup_cells :: Lens.Lens' UpdateRecoveryGroup [Prelude.Text]
updateRecoveryGroup_cells = Lens.lens (\UpdateRecoveryGroup' {cells} -> cells) (\s@UpdateRecoveryGroup' {} a -> s {cells = a} :: UpdateRecoveryGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRecoveryGroup where
  type
    AWSResponse UpdateRecoveryGroup =
      UpdateRecoveryGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecoveryGroupResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "recoveryGroupArn")
            Prelude.<*> (x Core..?> "recoveryGroupName")
            Prelude.<*> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRecoveryGroup where
  hashWithSalt _salt UpdateRecoveryGroup' {..} =
    _salt `Prelude.hashWithSalt` recoveryGroupName
      `Prelude.hashWithSalt` cells

instance Prelude.NFData UpdateRecoveryGroup where
  rnf UpdateRecoveryGroup' {..} =
    Prelude.rnf recoveryGroupName
      `Prelude.seq` Prelude.rnf cells

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
  { -- | The tags associated with the recovery group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) for the recovery group.
    recoveryGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the recovery group.
    recoveryGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of a cell\'s Amazon Resource Names (ARNs).
    cells :: Prelude.Maybe [Prelude.Text],
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
-- 'tags', 'updateRecoveryGroupResponse_tags' - The tags associated with the recovery group.
--
-- 'recoveryGroupArn', 'updateRecoveryGroupResponse_recoveryGroupArn' - The Amazon Resource Name (ARN) for the recovery group.
--
-- 'recoveryGroupName', 'updateRecoveryGroupResponse_recoveryGroupName' - The name of the recovery group.
--
-- 'cells', 'updateRecoveryGroupResponse_cells' - A list of a cell\'s Amazon Resource Names (ARNs).
--
-- 'httpStatus', 'updateRecoveryGroupResponse_httpStatus' - The response's http status code.
newUpdateRecoveryGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRecoveryGroupResponse
newUpdateRecoveryGroupResponse pHttpStatus_ =
  UpdateRecoveryGroupResponse'
    { tags =
        Prelude.Nothing,
      recoveryGroupArn = Prelude.Nothing,
      recoveryGroupName = Prelude.Nothing,
      cells = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags associated with the recovery group.
updateRecoveryGroupResponse_tags :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRecoveryGroupResponse_tags = Lens.lens (\UpdateRecoveryGroupResponse' {tags} -> tags) (\s@UpdateRecoveryGroupResponse' {} a -> s {tags = a} :: UpdateRecoveryGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the recovery group.
updateRecoveryGroupResponse_recoveryGroupArn :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe Prelude.Text)
updateRecoveryGroupResponse_recoveryGroupArn = Lens.lens (\UpdateRecoveryGroupResponse' {recoveryGroupArn} -> recoveryGroupArn) (\s@UpdateRecoveryGroupResponse' {} a -> s {recoveryGroupArn = a} :: UpdateRecoveryGroupResponse)

-- | The name of the recovery group.
updateRecoveryGroupResponse_recoveryGroupName :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe Prelude.Text)
updateRecoveryGroupResponse_recoveryGroupName = Lens.lens (\UpdateRecoveryGroupResponse' {recoveryGroupName} -> recoveryGroupName) (\s@UpdateRecoveryGroupResponse' {} a -> s {recoveryGroupName = a} :: UpdateRecoveryGroupResponse)

-- | A list of a cell\'s Amazon Resource Names (ARNs).
updateRecoveryGroupResponse_cells :: Lens.Lens' UpdateRecoveryGroupResponse (Prelude.Maybe [Prelude.Text])
updateRecoveryGroupResponse_cells = Lens.lens (\UpdateRecoveryGroupResponse' {cells} -> cells) (\s@UpdateRecoveryGroupResponse' {} a -> s {cells = a} :: UpdateRecoveryGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateRecoveryGroupResponse_httpStatus :: Lens.Lens' UpdateRecoveryGroupResponse Prelude.Int
updateRecoveryGroupResponse_httpStatus = Lens.lens (\UpdateRecoveryGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRecoveryGroupResponse' {} a -> s {httpStatus = a} :: UpdateRecoveryGroupResponse)

instance Prelude.NFData UpdateRecoveryGroupResponse where
  rnf UpdateRecoveryGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf recoveryGroupArn
      `Prelude.seq` Prelude.rnf recoveryGroupName
      `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf httpStatus
