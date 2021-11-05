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
-- Module      : Network.AWS.GlobalAccelerator.DeleteAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an accelerator. Before you can delete an accelerator, you must
-- disable it and remove all dependent resources (listeners and endpoint
-- groups). To disable the accelerator, update the accelerator to set
-- @Enabled@ to false.
--
-- When you create an accelerator, by default, Global Accelerator provides
-- you with a set of two static IP addresses. Alternatively, you can bring
-- your own IP address ranges to Global Accelerator and assign IP addresses
-- from those ranges.
--
-- The IP addresses are assigned to your accelerator for as long as it
-- exists, even if you disable the accelerator and it no longer accepts or
-- routes traffic. However, when you /delete/ an accelerator, you lose the
-- static IP addresses that are assigned to the accelerator, so you can no
-- longer route traffic by using them. As a best practice, ensure that you
-- have permissions in place to avoid inadvertently deleting accelerators.
-- You can use IAM policies with Global Accelerator to limit the users who
-- have permissions to delete an accelerator. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/auth-and-access-control.html Authentication and Access Control>
-- in the /AWS Global Accelerator Developer Guide/.
module Network.AWS.GlobalAccelerator.DeleteAccelerator
  ( -- * Creating a Request
    DeleteAccelerator (..),
    newDeleteAccelerator,

    -- * Request Lenses
    deleteAccelerator_acceleratorArn,

    -- * Destructuring the Response
    DeleteAcceleratorResponse (..),
    newDeleteAcceleratorResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GlobalAccelerator.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAccelerator' smart constructor.
data DeleteAccelerator = DeleteAccelerator'
  { -- | The Amazon Resource Name (ARN) of an accelerator.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'deleteAccelerator_acceleratorArn' - The Amazon Resource Name (ARN) of an accelerator.
newDeleteAccelerator ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  DeleteAccelerator
newDeleteAccelerator pAcceleratorArn_ =
  DeleteAccelerator'
    { acceleratorArn =
        pAcceleratorArn_
    }

-- | The Amazon Resource Name (ARN) of an accelerator.
deleteAccelerator_acceleratorArn :: Lens.Lens' DeleteAccelerator Prelude.Text
deleteAccelerator_acceleratorArn = Lens.lens (\DeleteAccelerator' {acceleratorArn} -> acceleratorArn) (\s@DeleteAccelerator' {} a -> s {acceleratorArn = a} :: DeleteAccelerator)

instance Core.AWSRequest DeleteAccelerator where
  type
    AWSResponse DeleteAccelerator =
      DeleteAcceleratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteAcceleratorResponse'

instance Prelude.Hashable DeleteAccelerator

instance Prelude.NFData DeleteAccelerator

instance Core.ToHeaders DeleteAccelerator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.DeleteAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAccelerator where
  toJSON DeleteAccelerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AcceleratorArn" Core..= acceleratorArn)
          ]
      )

instance Core.ToPath DeleteAccelerator where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAccelerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAcceleratorResponse' smart constructor.
data DeleteAcceleratorResponse = DeleteAcceleratorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAcceleratorResponse ::
  DeleteAcceleratorResponse
newDeleteAcceleratorResponse =
  DeleteAcceleratorResponse'

instance Prelude.NFData DeleteAcceleratorResponse
