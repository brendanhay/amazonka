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
-- Module      : Amazonka.DirectoryService.DisableRadius
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables multi-factor authentication (MFA) with the Remote
-- Authentication Dial In User Service (RADIUS) server for an AD Connector
-- or Microsoft AD directory.
module Amazonka.DirectoryService.DisableRadius
  ( -- * Creating a Request
    DisableRadius (..),
    newDisableRadius,

    -- * Request Lenses
    disableRadius_directoryId,

    -- * Destructuring the Response
    DisableRadiusResponse (..),
    newDisableRadiusResponse,

    -- * Response Lenses
    disableRadiusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DisableRadius operation.
--
-- /See:/ 'newDisableRadius' smart constructor.
data DisableRadius = DisableRadius'
  { -- | The identifier of the directory for which to disable MFA.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableRadius' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'disableRadius_directoryId' - The identifier of the directory for which to disable MFA.
newDisableRadius ::
  -- | 'directoryId'
  Prelude.Text ->
  DisableRadius
newDisableRadius pDirectoryId_ =
  DisableRadius' {directoryId = pDirectoryId_}

-- | The identifier of the directory for which to disable MFA.
disableRadius_directoryId :: Lens.Lens' DisableRadius Prelude.Text
disableRadius_directoryId = Lens.lens (\DisableRadius' {directoryId} -> directoryId) (\s@DisableRadius' {} a -> s {directoryId = a} :: DisableRadius)

instance Core.AWSRequest DisableRadius where
  type
    AWSResponse DisableRadius =
      DisableRadiusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableRadiusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableRadius where
  hashWithSalt _salt DisableRadius' {..} =
    _salt `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DisableRadius where
  rnf DisableRadius' {..} = Prelude.rnf directoryId

instance Data.ToHeaders DisableRadius where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DisableRadius" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableRadius where
  toJSON DisableRadius' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DirectoryId" Data..= directoryId)]
      )

instance Data.ToPath DisableRadius where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableRadius where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DisableRadius operation.
--
-- /See:/ 'newDisableRadiusResponse' smart constructor.
data DisableRadiusResponse = DisableRadiusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableRadiusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableRadiusResponse_httpStatus' - The response's http status code.
newDisableRadiusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableRadiusResponse
newDisableRadiusResponse pHttpStatus_ =
  DisableRadiusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableRadiusResponse_httpStatus :: Lens.Lens' DisableRadiusResponse Prelude.Int
disableRadiusResponse_httpStatus = Lens.lens (\DisableRadiusResponse' {httpStatus} -> httpStatus) (\s@DisableRadiusResponse' {} a -> s {httpStatus = a} :: DisableRadiusResponse)

instance Prelude.NFData DisableRadiusResponse where
  rnf DisableRadiusResponse' {..} =
    Prelude.rnf httpStatus
