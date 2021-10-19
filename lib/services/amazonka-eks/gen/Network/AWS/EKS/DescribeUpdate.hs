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
-- Module      : Network.AWS.EKS.DescribeUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an update against your Amazon EKS
-- cluster or associated managed node group.
--
-- When the status of the update is @Succeeded@, the update is complete. If
-- an update fails, the status is @Failed@, and an error detail explains
-- the reason for the failure.
module Network.AWS.EKS.DescribeUpdate
  ( -- * Creating a Request
    DescribeUpdate (..),
    newDescribeUpdate,

    -- * Request Lenses
    describeUpdate_addonName,
    describeUpdate_nodegroupName,
    describeUpdate_name,
    describeUpdate_updateId,

    -- * Destructuring the Response
    DescribeUpdateResponse (..),
    newDescribeUpdateResponse,

    -- * Response Lenses
    describeUpdateResponse_update,
    describeUpdateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUpdate' smart constructor.
data DescribeUpdate = DescribeUpdate'
  { -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EKS node group associated with the update.
    nodegroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EKS cluster associated with the update.
    name :: Prelude.Text,
    -- | The ID of the update to describe.
    updateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonName', 'describeUpdate_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
--
-- 'nodegroupName', 'describeUpdate_nodegroupName' - The name of the Amazon EKS node group associated with the update.
--
-- 'name', 'describeUpdate_name' - The name of the Amazon EKS cluster associated with the update.
--
-- 'updateId', 'describeUpdate_updateId' - The ID of the update to describe.
newDescribeUpdate ::
  -- | 'name'
  Prelude.Text ->
  -- | 'updateId'
  Prelude.Text ->
  DescribeUpdate
newDescribeUpdate pName_ pUpdateId_ =
  DescribeUpdate'
    { addonName = Prelude.Nothing,
      nodegroupName = Prelude.Nothing,
      name = pName_,
      updateId = pUpdateId_
    }

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
describeUpdate_addonName :: Lens.Lens' DescribeUpdate (Prelude.Maybe Prelude.Text)
describeUpdate_addonName = Lens.lens (\DescribeUpdate' {addonName} -> addonName) (\s@DescribeUpdate' {} a -> s {addonName = a} :: DescribeUpdate)

-- | The name of the Amazon EKS node group associated with the update.
describeUpdate_nodegroupName :: Lens.Lens' DescribeUpdate (Prelude.Maybe Prelude.Text)
describeUpdate_nodegroupName = Lens.lens (\DescribeUpdate' {nodegroupName} -> nodegroupName) (\s@DescribeUpdate' {} a -> s {nodegroupName = a} :: DescribeUpdate)

-- | The name of the Amazon EKS cluster associated with the update.
describeUpdate_name :: Lens.Lens' DescribeUpdate Prelude.Text
describeUpdate_name = Lens.lens (\DescribeUpdate' {name} -> name) (\s@DescribeUpdate' {} a -> s {name = a} :: DescribeUpdate)

-- | The ID of the update to describe.
describeUpdate_updateId :: Lens.Lens' DescribeUpdate Prelude.Text
describeUpdate_updateId = Lens.lens (\DescribeUpdate' {updateId} -> updateId) (\s@DescribeUpdate' {} a -> s {updateId = a} :: DescribeUpdate)

instance Core.AWSRequest DescribeUpdate where
  type
    AWSResponse DescribeUpdate =
      DescribeUpdateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUpdateResponse'
            Prelude.<$> (x Core..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUpdate

instance Prelude.NFData DescribeUpdate

instance Core.ToHeaders DescribeUpdate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeUpdate where
  toPath DescribeUpdate' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Core.toBS name,
        "/updates/",
        Core.toBS updateId
      ]

instance Core.ToQuery DescribeUpdate where
  toQuery DescribeUpdate' {..} =
    Prelude.mconcat
      [ "addonName" Core.=: addonName,
        "nodegroupName" Core.=: nodegroupName
      ]

-- | /See:/ 'newDescribeUpdateResponse' smart constructor.
data DescribeUpdateResponse = DescribeUpdateResponse'
  { -- | The full description of the specified update.
    update :: Prelude.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'describeUpdateResponse_update' - The full description of the specified update.
--
-- 'httpStatus', 'describeUpdateResponse_httpStatus' - The response's http status code.
newDescribeUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUpdateResponse
newDescribeUpdateResponse pHttpStatus_ =
  DescribeUpdateResponse'
    { update = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the specified update.
describeUpdateResponse_update :: Lens.Lens' DescribeUpdateResponse (Prelude.Maybe Update)
describeUpdateResponse_update = Lens.lens (\DescribeUpdateResponse' {update} -> update) (\s@DescribeUpdateResponse' {} a -> s {update = a} :: DescribeUpdateResponse)

-- | The response's http status code.
describeUpdateResponse_httpStatus :: Lens.Lens' DescribeUpdateResponse Prelude.Int
describeUpdateResponse_httpStatus = Lens.lens (\DescribeUpdateResponse' {httpStatus} -> httpStatus) (\s@DescribeUpdateResponse' {} a -> s {httpStatus = a} :: DescribeUpdateResponse)

instance Prelude.NFData DescribeUpdateResponse
