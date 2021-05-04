{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.DeleteVPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for your Amazon Virtual Private Cloud (VPC)
-- endpoint.
module Network.AWS.DeviceFarm.DeleteVPCEConfiguration
  ( -- * Creating a Request
    DeleteVPCEConfiguration (..),
    newDeleteVPCEConfiguration,

    -- * Request Lenses
    deleteVPCEConfiguration_arn,

    -- * Destructuring the Response
    DeleteVPCEConfigurationResponse (..),
    newDeleteVPCEConfigurationResponse,

    -- * Response Lenses
    deleteVPCEConfigurationResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVPCEConfiguration' smart constructor.
data DeleteVPCEConfiguration = DeleteVPCEConfiguration'
  { -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
    -- want to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteVPCEConfiguration_arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to delete.
newDeleteVPCEConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  DeleteVPCEConfiguration
newDeleteVPCEConfiguration pArn_ =
  DeleteVPCEConfiguration' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to delete.
deleteVPCEConfiguration_arn :: Lens.Lens' DeleteVPCEConfiguration Prelude.Text
deleteVPCEConfiguration_arn = Lens.lens (\DeleteVPCEConfiguration' {arn} -> arn) (\s@DeleteVPCEConfiguration' {} a -> s {arn = a} :: DeleteVPCEConfiguration)

instance Prelude.AWSRequest DeleteVPCEConfiguration where
  type
    Rs DeleteVPCEConfiguration =
      DeleteVPCEConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVPCEConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVPCEConfiguration

instance Prelude.NFData DeleteVPCEConfiguration

instance Prelude.ToHeaders DeleteVPCEConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.DeleteVPCEConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteVPCEConfiguration where
  toJSON DeleteVPCEConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteVPCEConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVPCEConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVPCEConfigurationResponse' smart constructor.
data DeleteVPCEConfigurationResponse = DeleteVPCEConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVPCEConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVPCEConfigurationResponse_httpStatus' - The response's http status code.
newDeleteVPCEConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVPCEConfigurationResponse
newDeleteVPCEConfigurationResponse pHttpStatus_ =
  DeleteVPCEConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteVPCEConfigurationResponse_httpStatus :: Lens.Lens' DeleteVPCEConfigurationResponse Prelude.Int
deleteVPCEConfigurationResponse_httpStatus = Lens.lens (\DeleteVPCEConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteVPCEConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteVPCEConfigurationResponse)

instance
  Prelude.NFData
    DeleteVPCEConfigurationResponse
