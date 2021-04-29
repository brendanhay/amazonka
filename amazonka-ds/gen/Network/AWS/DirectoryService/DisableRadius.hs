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
-- Module      : Network.AWS.DirectoryService.DisableRadius
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables multi-factor authentication (MFA) with the Remote
-- Authentication Dial In User Service (RADIUS) server for an AD Connector
-- or Microsoft AD directory.
module Network.AWS.DirectoryService.DisableRadius
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DisableRadius operation.
--
-- /See:/ 'newDisableRadius' smart constructor.
data DisableRadius = DisableRadius'
  { -- | The identifier of the directory for which to disable MFA.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DisableRadius where
  type Rs DisableRadius = DisableRadiusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableRadiusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableRadius

instance Prelude.NFData DisableRadius

instance Prelude.ToHeaders DisableRadius where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DisableRadius" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableRadius where
  toJSON DisableRadius' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath DisableRadius where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableRadius where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DisableRadius operation.
--
-- /See:/ 'newDisableRadiusResponse' smart constructor.
data DisableRadiusResponse = DisableRadiusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DisableRadiusResponse
