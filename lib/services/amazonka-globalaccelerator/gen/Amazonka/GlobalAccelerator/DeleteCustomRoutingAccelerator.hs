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
-- Module      : Amazonka.GlobalAccelerator.DeleteCustomRoutingAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom routing accelerator. Before you can delete an
-- accelerator, you must disable it and remove all dependent resources
-- (listeners and endpoint groups). To disable the accelerator, update the
-- accelerator to set @Enabled@ to false.
--
-- When you create a custom routing accelerator, by default, Global
-- Accelerator provides you with a set of two static IP addresses.
--
-- The IP addresses are assigned to your accelerator for as long as it
-- exists, even if you disable the accelerator and it no longer accepts or
-- routes traffic. However, when you /delete/ an accelerator, you lose the
-- static IP addresses that are assigned to the accelerator, so you can no
-- longer route traffic by using them. As a best practice, ensure that you
-- have permissions in place to avoid inadvertently deleting accelerators.
-- You can use IAM policies with Global Accelerator to limit the users who
-- have permissions to delete an accelerator. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/auth-and-access-control.html Identity and access management>
-- in the /Global Accelerator Developer Guide/.
module Amazonka.GlobalAccelerator.DeleteCustomRoutingAccelerator
  ( -- * Creating a Request
    DeleteCustomRoutingAccelerator (..),
    newDeleteCustomRoutingAccelerator,

    -- * Request Lenses
    deleteCustomRoutingAccelerator_acceleratorArn,

    -- * Destructuring the Response
    DeleteCustomRoutingAcceleratorResponse (..),
    newDeleteCustomRoutingAcceleratorResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomRoutingAccelerator' smart constructor.
data DeleteCustomRoutingAccelerator = DeleteCustomRoutingAccelerator'
  { -- | The Amazon Resource Name (ARN) of the custom routing accelerator to
    -- delete.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'deleteCustomRoutingAccelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator to
-- delete.
newDeleteCustomRoutingAccelerator ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  DeleteCustomRoutingAccelerator
newDeleteCustomRoutingAccelerator pAcceleratorArn_ =
  DeleteCustomRoutingAccelerator'
    { acceleratorArn =
        pAcceleratorArn_
    }

-- | The Amazon Resource Name (ARN) of the custom routing accelerator to
-- delete.
deleteCustomRoutingAccelerator_acceleratorArn :: Lens.Lens' DeleteCustomRoutingAccelerator Prelude.Text
deleteCustomRoutingAccelerator_acceleratorArn = Lens.lens (\DeleteCustomRoutingAccelerator' {acceleratorArn} -> acceleratorArn) (\s@DeleteCustomRoutingAccelerator' {} a -> s {acceleratorArn = a} :: DeleteCustomRoutingAccelerator)

instance
  Core.AWSRequest
    DeleteCustomRoutingAccelerator
  where
  type
    AWSResponse DeleteCustomRoutingAccelerator =
      DeleteCustomRoutingAcceleratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCustomRoutingAcceleratorResponse'

instance
  Prelude.Hashable
    DeleteCustomRoutingAccelerator
  where
  hashWithSalt
    _salt
    DeleteCustomRoutingAccelerator' {..} =
      _salt `Prelude.hashWithSalt` acceleratorArn

instance
  Prelude.NFData
    DeleteCustomRoutingAccelerator
  where
  rnf DeleteCustomRoutingAccelerator' {..} =
    Prelude.rnf acceleratorArn

instance
  Data.ToHeaders
    DeleteCustomRoutingAccelerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DeleteCustomRoutingAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCustomRoutingAccelerator where
  toJSON DeleteCustomRoutingAccelerator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AcceleratorArn" Data..= acceleratorArn)
          ]
      )

instance Data.ToPath DeleteCustomRoutingAccelerator where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCustomRoutingAccelerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomRoutingAcceleratorResponse' smart constructor.
data DeleteCustomRoutingAcceleratorResponse = DeleteCustomRoutingAcceleratorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomRoutingAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomRoutingAcceleratorResponse ::
  DeleteCustomRoutingAcceleratorResponse
newDeleteCustomRoutingAcceleratorResponse =
  DeleteCustomRoutingAcceleratorResponse'

instance
  Prelude.NFData
    DeleteCustomRoutingAcceleratorResponse
  where
  rnf _ = ()
