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
-- Module      : Network.AWS.DAX.CreateSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new subnet group.
module Network.AWS.DAX.CreateSubnetGroup
  ( -- * Creating a Request
    CreateSubnetGroup (..),
    newCreateSubnetGroup,

    -- * Request Lenses
    createSubnetGroup_description,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,

    -- * Destructuring the Response
    CreateSubnetGroupResponse (..),
    newCreateSubnetGroupResponse,

    -- * Response Lenses
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubnetGroup' smart constructor.
data CreateSubnetGroup = CreateSubnetGroup'
  { -- | A description for the subnet group
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the subnet group. This value is stored as a lowercase string.
    subnetGroupName :: Prelude.Text,
    -- | A list of VPC subnet IDs for the subnet group.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSubnetGroup_description' - A description for the subnet group
--
-- 'subnetGroupName', 'createSubnetGroup_subnetGroupName' - A name for the subnet group. This value is stored as a lowercase string.
--
-- 'subnetIds', 'createSubnetGroup_subnetIds' - A list of VPC subnet IDs for the subnet group.
newCreateSubnetGroup ::
  -- | 'subnetGroupName'
  Prelude.Text ->
  CreateSubnetGroup
newCreateSubnetGroup pSubnetGroupName_ =
  CreateSubnetGroup'
    { description = Prelude.Nothing,
      subnetGroupName = pSubnetGroupName_,
      subnetIds = Prelude.mempty
    }

-- | A description for the subnet group
createSubnetGroup_description :: Lens.Lens' CreateSubnetGroup (Prelude.Maybe Prelude.Text)
createSubnetGroup_description = Lens.lens (\CreateSubnetGroup' {description} -> description) (\s@CreateSubnetGroup' {} a -> s {description = a} :: CreateSubnetGroup)

-- | A name for the subnet group. This value is stored as a lowercase string.
createSubnetGroup_subnetGroupName :: Lens.Lens' CreateSubnetGroup Prelude.Text
createSubnetGroup_subnetGroupName = Lens.lens (\CreateSubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@CreateSubnetGroup' {} a -> s {subnetGroupName = a} :: CreateSubnetGroup)

-- | A list of VPC subnet IDs for the subnet group.
createSubnetGroup_subnetIds :: Lens.Lens' CreateSubnetGroup [Prelude.Text]
createSubnetGroup_subnetIds = Lens.lens (\CreateSubnetGroup' {subnetIds} -> subnetIds) (\s@CreateSubnetGroup' {} a -> s {subnetIds = a} :: CreateSubnetGroup) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateSubnetGroup where
  type
    AWSResponse CreateSubnetGroup =
      CreateSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubnetGroupResponse'
            Prelude.<$> (x Core..?> "SubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubnetGroup

instance Prelude.NFData CreateSubnetGroup

instance Core.ToHeaders CreateSubnetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.CreateSubnetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSubnetGroup where
  toJSON CreateSubnetGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            Prelude.Just
              ("SubnetGroupName" Core..= subnetGroupName),
            Prelude.Just ("SubnetIds" Core..= subnetIds)
          ]
      )

instance Core.ToPath CreateSubnetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSubnetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubnetGroupResponse' smart constructor.
data CreateSubnetGroupResponse = CreateSubnetGroupResponse'
  { -- | Represents the output of a /CreateSubnetGroup/ operation.
    subnetGroup :: Prelude.Maybe SubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroup', 'createSubnetGroupResponse_subnetGroup' - Represents the output of a /CreateSubnetGroup/ operation.
--
-- 'httpStatus', 'createSubnetGroupResponse_httpStatus' - The response's http status code.
newCreateSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubnetGroupResponse
newCreateSubnetGroupResponse pHttpStatus_ =
  CreateSubnetGroupResponse'
    { subnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the output of a /CreateSubnetGroup/ operation.
createSubnetGroupResponse_subnetGroup :: Lens.Lens' CreateSubnetGroupResponse (Prelude.Maybe SubnetGroup)
createSubnetGroupResponse_subnetGroup = Lens.lens (\CreateSubnetGroupResponse' {subnetGroup} -> subnetGroup) (\s@CreateSubnetGroupResponse' {} a -> s {subnetGroup = a} :: CreateSubnetGroupResponse)

-- | The response's http status code.
createSubnetGroupResponse_httpStatus :: Lens.Lens' CreateSubnetGroupResponse Prelude.Int
createSubnetGroupResponse_httpStatus = Lens.lens (\CreateSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateSubnetGroupResponse' {} a -> s {httpStatus = a} :: CreateSubnetGroupResponse)

instance Prelude.NFData CreateSubnetGroupResponse
