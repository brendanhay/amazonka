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
-- Module      : Amazonka.AppRunner.DeleteObservabilityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an App Runner observability configuration resource. You can
-- delete a specific revision or the latest active revision. You can\'t
-- delete a configuration that\'s used by one or more App Runner services.
module Amazonka.AppRunner.DeleteObservabilityConfiguration
  ( -- * Creating a Request
    DeleteObservabilityConfiguration (..),
    newDeleteObservabilityConfiguration,

    -- * Request Lenses
    deleteObservabilityConfiguration_observabilityConfigurationArn,

    -- * Destructuring the Response
    DeleteObservabilityConfigurationResponse (..),
    newDeleteObservabilityConfigurationResponse,

    -- * Response Lenses
    deleteObservabilityConfigurationResponse_httpStatus,
    deleteObservabilityConfigurationResponse_observabilityConfiguration,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteObservabilityConfiguration' smart constructor.
data DeleteObservabilityConfiguration = DeleteObservabilityConfiguration'
  { -- | The Amazon Resource Name (ARN) of the App Runner observability
    -- configuration that you want to delete.
    --
    -- The ARN can be a full observability configuration ARN, or a partial ARN
    -- ending with either @...\/name @ or @...\/name\/revision @. If a revision
    -- isn\'t specified, the latest active revision is deleted.
    observabilityConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObservabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observabilityConfigurationArn', 'deleteObservabilityConfiguration_observabilityConfigurationArn' - The Amazon Resource Name (ARN) of the App Runner observability
-- configuration that you want to delete.
--
-- The ARN can be a full observability configuration ARN, or a partial ARN
-- ending with either @...\/name @ or @...\/name\/revision @. If a revision
-- isn\'t specified, the latest active revision is deleted.
newDeleteObservabilityConfiguration ::
  -- | 'observabilityConfigurationArn'
  Prelude.Text ->
  DeleteObservabilityConfiguration
newDeleteObservabilityConfiguration
  pObservabilityConfigurationArn_ =
    DeleteObservabilityConfiguration'
      { observabilityConfigurationArn =
          pObservabilityConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the App Runner observability
-- configuration that you want to delete.
--
-- The ARN can be a full observability configuration ARN, or a partial ARN
-- ending with either @...\/name @ or @...\/name\/revision @. If a revision
-- isn\'t specified, the latest active revision is deleted.
deleteObservabilityConfiguration_observabilityConfigurationArn :: Lens.Lens' DeleteObservabilityConfiguration Prelude.Text
deleteObservabilityConfiguration_observabilityConfigurationArn = Lens.lens (\DeleteObservabilityConfiguration' {observabilityConfigurationArn} -> observabilityConfigurationArn) (\s@DeleteObservabilityConfiguration' {} a -> s {observabilityConfigurationArn = a} :: DeleteObservabilityConfiguration)

instance
  Core.AWSRequest
    DeleteObservabilityConfiguration
  where
  type
    AWSResponse DeleteObservabilityConfiguration =
      DeleteObservabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteObservabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ObservabilityConfiguration")
      )

instance
  Prelude.Hashable
    DeleteObservabilityConfiguration
  where
  hashWithSalt
    _salt
    DeleteObservabilityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` observabilityConfigurationArn

instance
  Prelude.NFData
    DeleteObservabilityConfiguration
  where
  rnf DeleteObservabilityConfiguration' {..} =
    Prelude.rnf observabilityConfigurationArn

instance
  Core.ToHeaders
    DeleteObservabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AppRunner.DeleteObservabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteObservabilityConfiguration where
  toJSON DeleteObservabilityConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ObservabilityConfigurationArn"
                  Core..= observabilityConfigurationArn
              )
          ]
      )

instance Core.ToPath DeleteObservabilityConfiguration where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteObservabilityConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteObservabilityConfigurationResponse' smart constructor.
data DeleteObservabilityConfigurationResponse = DeleteObservabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner observability configuration that this
    -- request just deleted.
    observabilityConfiguration :: ObservabilityConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObservabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteObservabilityConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'observabilityConfiguration', 'deleteObservabilityConfigurationResponse_observabilityConfiguration' - A description of the App Runner observability configuration that this
-- request just deleted.
newDeleteObservabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'observabilityConfiguration'
  ObservabilityConfiguration ->
  DeleteObservabilityConfigurationResponse
newDeleteObservabilityConfigurationResponse
  pHttpStatus_
  pObservabilityConfiguration_ =
    DeleteObservabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        observabilityConfiguration =
          pObservabilityConfiguration_
      }

-- | The response's http status code.
deleteObservabilityConfigurationResponse_httpStatus :: Lens.Lens' DeleteObservabilityConfigurationResponse Prelude.Int
deleteObservabilityConfigurationResponse_httpStatus = Lens.lens (\DeleteObservabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteObservabilityConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteObservabilityConfigurationResponse)

-- | A description of the App Runner observability configuration that this
-- request just deleted.
deleteObservabilityConfigurationResponse_observabilityConfiguration :: Lens.Lens' DeleteObservabilityConfigurationResponse ObservabilityConfiguration
deleteObservabilityConfigurationResponse_observabilityConfiguration = Lens.lens (\DeleteObservabilityConfigurationResponse' {observabilityConfiguration} -> observabilityConfiguration) (\s@DeleteObservabilityConfigurationResponse' {} a -> s {observabilityConfiguration = a} :: DeleteObservabilityConfigurationResponse)

instance
  Prelude.NFData
    DeleteObservabilityConfigurationResponse
  where
  rnf DeleteObservabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf observabilityConfiguration
