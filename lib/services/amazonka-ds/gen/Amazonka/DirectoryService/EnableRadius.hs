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
-- Module      : Amazonka.DirectoryService.EnableRadius
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables multi-factor authentication (MFA) with the Remote Authentication
-- Dial In User Service (RADIUS) server for an AD Connector or Microsoft AD
-- directory.
module Amazonka.DirectoryService.EnableRadius
  ( -- * Creating a Request
    EnableRadius (..),
    newEnableRadius,

    -- * Request Lenses
    enableRadius_directoryId,
    enableRadius_radiusSettings,

    -- * Destructuring the Response
    EnableRadiusResponse (..),
    newEnableRadiusResponse,

    -- * Response Lenses
    enableRadiusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the EnableRadius operation.
--
-- /See:/ 'newEnableRadius' smart constructor.
data EnableRadius = EnableRadius'
  { -- | The identifier of the directory for which to enable MFA.
    directoryId :: Prelude.Text,
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server.
    radiusSettings :: RadiusSettings
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableRadius' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'enableRadius_directoryId' - The identifier of the directory for which to enable MFA.
--
-- 'radiusSettings', 'enableRadius_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server.
newEnableRadius ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'radiusSettings'
  RadiusSettings ->
  EnableRadius
newEnableRadius pDirectoryId_ pRadiusSettings_ =
  EnableRadius'
    { directoryId = pDirectoryId_,
      radiusSettings = pRadiusSettings_
    }

-- | The identifier of the directory for which to enable MFA.
enableRadius_directoryId :: Lens.Lens' EnableRadius Prelude.Text
enableRadius_directoryId = Lens.lens (\EnableRadius' {directoryId} -> directoryId) (\s@EnableRadius' {} a -> s {directoryId = a} :: EnableRadius)

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
enableRadius_radiusSettings :: Lens.Lens' EnableRadius RadiusSettings
enableRadius_radiusSettings = Lens.lens (\EnableRadius' {radiusSettings} -> radiusSettings) (\s@EnableRadius' {} a -> s {radiusSettings = a} :: EnableRadius)

instance Core.AWSRequest EnableRadius where
  type AWSResponse EnableRadius = EnableRadiusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableRadiusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableRadius where
  hashWithSalt _salt EnableRadius' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` radiusSettings

instance Prelude.NFData EnableRadius where
  rnf EnableRadius' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf radiusSettings

instance Core.ToHeaders EnableRadius where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.EnableRadius" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON EnableRadius where
  toJSON EnableRadius' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Core..= directoryId),
            Prelude.Just
              ("RadiusSettings" Core..= radiusSettings)
          ]
      )

instance Core.ToPath EnableRadius where
  toPath = Prelude.const "/"

instance Core.ToQuery EnableRadius where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the EnableRadius operation.
--
-- /See:/ 'newEnableRadiusResponse' smart constructor.
data EnableRadiusResponse = EnableRadiusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableRadiusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableRadiusResponse_httpStatus' - The response's http status code.
newEnableRadiusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableRadiusResponse
newEnableRadiusResponse pHttpStatus_ =
  EnableRadiusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableRadiusResponse_httpStatus :: Lens.Lens' EnableRadiusResponse Prelude.Int
enableRadiusResponse_httpStatus = Lens.lens (\EnableRadiusResponse' {httpStatus} -> httpStatus) (\s@EnableRadiusResponse' {} a -> s {httpStatus = a} :: EnableRadiusResponse)

instance Prelude.NFData EnableRadiusResponse where
  rnf EnableRadiusResponse' {..} =
    Prelude.rnf httpStatus
