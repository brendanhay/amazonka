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
-- Module      : Amazonka.AppRunner.DeleteAutoScalingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an App Runner automatic scaling configuration resource. You can
-- delete a specific revision or the latest active revision. You can\'t
-- delete a configuration that\'s used by one or more App Runner services.
module Amazonka.AppRunner.DeleteAutoScalingConfiguration
  ( -- * Creating a Request
    DeleteAutoScalingConfiguration (..),
    newDeleteAutoScalingConfiguration,

    -- * Request Lenses
    deleteAutoScalingConfiguration_autoScalingConfigurationArn,

    -- * Destructuring the Response
    DeleteAutoScalingConfigurationResponse (..),
    newDeleteAutoScalingConfigurationResponse,

    -- * Response Lenses
    deleteAutoScalingConfigurationResponse_httpStatus,
    deleteAutoScalingConfigurationResponse_autoScalingConfiguration,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAutoScalingConfiguration' smart constructor.
data DeleteAutoScalingConfiguration = DeleteAutoScalingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the App Runner auto scaling
    -- configuration that you want to delete.
    --
    -- The ARN can be a full auto scaling configuration ARN, or a partial ARN
    -- ending with either @...\/name @ or @...\/name\/revision @. If a revision
    -- isn\'t specified, the latest active revision is deleted.
    autoScalingConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfigurationArn', 'deleteAutoScalingConfiguration_autoScalingConfigurationArn' - The Amazon Resource Name (ARN) of the App Runner auto scaling
-- configuration that you want to delete.
--
-- The ARN can be a full auto scaling configuration ARN, or a partial ARN
-- ending with either @...\/name @ or @...\/name\/revision @. If a revision
-- isn\'t specified, the latest active revision is deleted.
newDeleteAutoScalingConfiguration ::
  -- | 'autoScalingConfigurationArn'
  Prelude.Text ->
  DeleteAutoScalingConfiguration
newDeleteAutoScalingConfiguration
  pAutoScalingConfigurationArn_ =
    DeleteAutoScalingConfiguration'
      { autoScalingConfigurationArn =
          pAutoScalingConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the App Runner auto scaling
-- configuration that you want to delete.
--
-- The ARN can be a full auto scaling configuration ARN, or a partial ARN
-- ending with either @...\/name @ or @...\/name\/revision @. If a revision
-- isn\'t specified, the latest active revision is deleted.
deleteAutoScalingConfiguration_autoScalingConfigurationArn :: Lens.Lens' DeleteAutoScalingConfiguration Prelude.Text
deleteAutoScalingConfiguration_autoScalingConfigurationArn = Lens.lens (\DeleteAutoScalingConfiguration' {autoScalingConfigurationArn} -> autoScalingConfigurationArn) (\s@DeleteAutoScalingConfiguration' {} a -> s {autoScalingConfigurationArn = a} :: DeleteAutoScalingConfiguration)

instance
  Core.AWSRequest
    DeleteAutoScalingConfiguration
  where
  type
    AWSResponse DeleteAutoScalingConfiguration =
      DeleteAutoScalingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAutoScalingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AutoScalingConfiguration")
      )

instance
  Prelude.Hashable
    DeleteAutoScalingConfiguration
  where
  hashWithSalt
    _salt
    DeleteAutoScalingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoScalingConfigurationArn

instance
  Prelude.NFData
    DeleteAutoScalingConfiguration
  where
  rnf DeleteAutoScalingConfiguration' {..} =
    Prelude.rnf autoScalingConfigurationArn

instance
  Core.ToHeaders
    DeleteAutoScalingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AppRunner.DeleteAutoScalingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAutoScalingConfiguration where
  toJSON DeleteAutoScalingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AutoScalingConfigurationArn"
                  Core..= autoScalingConfigurationArn
              )
          ]
      )

instance Core.ToPath DeleteAutoScalingConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAutoScalingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAutoScalingConfigurationResponse' smart constructor.
data DeleteAutoScalingConfigurationResponse = DeleteAutoScalingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner auto scaling configuration that this
    -- request just deleted.
    autoScalingConfiguration :: AutoScalingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAutoScalingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAutoScalingConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'autoScalingConfiguration', 'deleteAutoScalingConfigurationResponse_autoScalingConfiguration' - A description of the App Runner auto scaling configuration that this
-- request just deleted.
newDeleteAutoScalingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoScalingConfiguration'
  AutoScalingConfiguration ->
  DeleteAutoScalingConfigurationResponse
newDeleteAutoScalingConfigurationResponse
  pHttpStatus_
  pAutoScalingConfiguration_ =
    DeleteAutoScalingConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        autoScalingConfiguration =
          pAutoScalingConfiguration_
      }

-- | The response's http status code.
deleteAutoScalingConfigurationResponse_httpStatus :: Lens.Lens' DeleteAutoScalingConfigurationResponse Prelude.Int
deleteAutoScalingConfigurationResponse_httpStatus = Lens.lens (\DeleteAutoScalingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAutoScalingConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAutoScalingConfigurationResponse)

-- | A description of the App Runner auto scaling configuration that this
-- request just deleted.
deleteAutoScalingConfigurationResponse_autoScalingConfiguration :: Lens.Lens' DeleteAutoScalingConfigurationResponse AutoScalingConfiguration
deleteAutoScalingConfigurationResponse_autoScalingConfiguration = Lens.lens (\DeleteAutoScalingConfigurationResponse' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@DeleteAutoScalingConfigurationResponse' {} a -> s {autoScalingConfiguration = a} :: DeleteAutoScalingConfigurationResponse)

instance
  Prelude.NFData
    DeleteAutoScalingConfigurationResponse
  where
  rnf DeleteAutoScalingConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoScalingConfiguration
