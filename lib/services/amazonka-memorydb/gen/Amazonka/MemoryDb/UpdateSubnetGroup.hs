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
-- Module      : Amazonka.MemoryDb.UpdateSubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subnet group. For more information, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/ubnetGroups.Modifying.html Updating a subnet group>
module Amazonka.MemoryDb.UpdateSubnetGroup
  ( -- * Creating a Request
    UpdateSubnetGroup (..),
    newUpdateSubnetGroup,

    -- * Request Lenses
    updateSubnetGroup_description,
    updateSubnetGroup_subnetIds,
    updateSubnetGroup_subnetGroupName,

    -- * Destructuring the Response
    UpdateSubnetGroupResponse (..),
    newUpdateSubnetGroupResponse,

    -- * Response Lenses
    updateSubnetGroupResponse_subnetGroup,
    updateSubnetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSubnetGroup' smart constructor.
data UpdateSubnetGroup = UpdateSubnetGroup'
  { -- | A description of the subnet group
    description :: Prelude.Maybe Prelude.Text,
    -- | The EC2 subnet IDs for the subnet group.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the subnet group
    subnetGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateSubnetGroup_description' - A description of the subnet group
--
-- 'subnetIds', 'updateSubnetGroup_subnetIds' - The EC2 subnet IDs for the subnet group.
--
-- 'subnetGroupName', 'updateSubnetGroup_subnetGroupName' - The name of the subnet group
newUpdateSubnetGroup ::
  -- | 'subnetGroupName'
  Prelude.Text ->
  UpdateSubnetGroup
newUpdateSubnetGroup pSubnetGroupName_ =
  UpdateSubnetGroup'
    { description = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      subnetGroupName = pSubnetGroupName_
    }

-- | A description of the subnet group
updateSubnetGroup_description :: Lens.Lens' UpdateSubnetGroup (Prelude.Maybe Prelude.Text)
updateSubnetGroup_description = Lens.lens (\UpdateSubnetGroup' {description} -> description) (\s@UpdateSubnetGroup' {} a -> s {description = a} :: UpdateSubnetGroup)

-- | The EC2 subnet IDs for the subnet group.
updateSubnetGroup_subnetIds :: Lens.Lens' UpdateSubnetGroup (Prelude.Maybe [Prelude.Text])
updateSubnetGroup_subnetIds = Lens.lens (\UpdateSubnetGroup' {subnetIds} -> subnetIds) (\s@UpdateSubnetGroup' {} a -> s {subnetIds = a} :: UpdateSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the subnet group
updateSubnetGroup_subnetGroupName :: Lens.Lens' UpdateSubnetGroup Prelude.Text
updateSubnetGroup_subnetGroupName = Lens.lens (\UpdateSubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@UpdateSubnetGroup' {} a -> s {subnetGroupName = a} :: UpdateSubnetGroup)

instance Core.AWSRequest UpdateSubnetGroup where
  type
    AWSResponse UpdateSubnetGroup =
      UpdateSubnetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubnetGroupResponse'
            Prelude.<$> (x Core..?> "SubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSubnetGroup where
  hashWithSalt _salt UpdateSubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` subnetGroupName

instance Prelude.NFData UpdateSubnetGroup where
  rnf UpdateSubnetGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf subnetGroupName

instance Core.ToHeaders UpdateSubnetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.UpdateSubnetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSubnetGroup where
  toJSON UpdateSubnetGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            ("SubnetIds" Core..=) Prelude.<$> subnetIds,
            Prelude.Just
              ("SubnetGroupName" Core..= subnetGroupName)
          ]
      )

instance Core.ToPath UpdateSubnetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSubnetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubnetGroupResponse' smart constructor.
data UpdateSubnetGroupResponse = UpdateSubnetGroupResponse'
  { -- | The updated subnet group
    subnetGroup :: Prelude.Maybe SubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroup', 'updateSubnetGroupResponse_subnetGroup' - The updated subnet group
--
-- 'httpStatus', 'updateSubnetGroupResponse_httpStatus' - The response's http status code.
newUpdateSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubnetGroupResponse
newUpdateSubnetGroupResponse pHttpStatus_ =
  UpdateSubnetGroupResponse'
    { subnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated subnet group
updateSubnetGroupResponse_subnetGroup :: Lens.Lens' UpdateSubnetGroupResponse (Prelude.Maybe SubnetGroup)
updateSubnetGroupResponse_subnetGroup = Lens.lens (\UpdateSubnetGroupResponse' {subnetGroup} -> subnetGroup) (\s@UpdateSubnetGroupResponse' {} a -> s {subnetGroup = a} :: UpdateSubnetGroupResponse)

-- | The response's http status code.
updateSubnetGroupResponse_httpStatus :: Lens.Lens' UpdateSubnetGroupResponse Prelude.Int
updateSubnetGroupResponse_httpStatus = Lens.lens (\UpdateSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateSubnetGroupResponse' {} a -> s {httpStatus = a} :: UpdateSubnetGroupResponse)

instance Prelude.NFData UpdateSubnetGroupResponse where
  rnf UpdateSubnetGroupResponse' {..} =
    Prelude.rnf subnetGroup
      `Prelude.seq` Prelude.rnf httpStatus
