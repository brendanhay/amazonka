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
-- Module      : Amazonka.Lightsail.CreateGUISessionAccessDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates two URLs that are used to access a virtual computer’s graphical
-- user interface (GUI) session. The primary URL initiates a web-based NICE
-- DCV session to the virtual computer\'s application. The secondary URL
-- initiates a web-based NICE DCV session to the virtual computer\'s
-- operating session.
--
-- Use @StartGUISession@ to open the session.
module Amazonka.Lightsail.CreateGUISessionAccessDetails
  ( -- * Creating a Request
    CreateGUISessionAccessDetails (..),
    newCreateGUISessionAccessDetails,

    -- * Request Lenses
    createGUISessionAccessDetails_resourceName,

    -- * Destructuring the Response
    CreateGUISessionAccessDetailsResponse (..),
    newCreateGUISessionAccessDetailsResponse,

    -- * Response Lenses
    createGUISessionAccessDetailsResponse_failureReason,
    createGUISessionAccessDetailsResponse_percentageComplete,
    createGUISessionAccessDetailsResponse_resourceName,
    createGUISessionAccessDetailsResponse_sessions,
    createGUISessionAccessDetailsResponse_status,
    createGUISessionAccessDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGUISessionAccessDetails' smart constructor.
data CreateGUISessionAccessDetails = CreateGUISessionAccessDetails'
  { -- | The resource name.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGUISessionAccessDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'createGUISessionAccessDetails_resourceName' - The resource name.
newCreateGUISessionAccessDetails ::
  -- | 'resourceName'
  Prelude.Text ->
  CreateGUISessionAccessDetails
newCreateGUISessionAccessDetails pResourceName_ =
  CreateGUISessionAccessDetails'
    { resourceName =
        pResourceName_
    }

-- | The resource name.
createGUISessionAccessDetails_resourceName :: Lens.Lens' CreateGUISessionAccessDetails Prelude.Text
createGUISessionAccessDetails_resourceName = Lens.lens (\CreateGUISessionAccessDetails' {resourceName} -> resourceName) (\s@CreateGUISessionAccessDetails' {} a -> s {resourceName = a} :: CreateGUISessionAccessDetails)

instance
  Core.AWSRequest
    CreateGUISessionAccessDetails
  where
  type
    AWSResponse CreateGUISessionAccessDetails =
      CreateGUISessionAccessDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGUISessionAccessDetailsResponse'
            Prelude.<$> (x Data..?> "failureReason")
            Prelude.<*> (x Data..?> "percentageComplete")
            Prelude.<*> (x Data..?> "resourceName")
            Prelude.<*> (x Data..?> "sessions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateGUISessionAccessDetails
  where
  hashWithSalt _salt CreateGUISessionAccessDetails' {..} =
    _salt `Prelude.hashWithSalt` resourceName

instance Prelude.NFData CreateGUISessionAccessDetails where
  rnf CreateGUISessionAccessDetails' {..} =
    Prelude.rnf resourceName

instance Data.ToHeaders CreateGUISessionAccessDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateGUISessionAccessDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGUISessionAccessDetails where
  toJSON CreateGUISessionAccessDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath CreateGUISessionAccessDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGUISessionAccessDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGUISessionAccessDetailsResponse' smart constructor.
data CreateGUISessionAccessDetailsResponse = CreateGUISessionAccessDetailsResponse'
  { -- | The reason the operation failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The percentage of completion for the operation.
    percentageComplete :: Prelude.Maybe Prelude.Int,
    -- | The resource name.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | Returns information about the specified NICE DCV GUI session.
    sessions :: Prelude.Maybe [Session],
    -- | The status of the operation.
    status :: Prelude.Maybe Status,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGUISessionAccessDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'createGUISessionAccessDetailsResponse_failureReason' - The reason the operation failed.
--
-- 'percentageComplete', 'createGUISessionAccessDetailsResponse_percentageComplete' - The percentage of completion for the operation.
--
-- 'resourceName', 'createGUISessionAccessDetailsResponse_resourceName' - The resource name.
--
-- 'sessions', 'createGUISessionAccessDetailsResponse_sessions' - Returns information about the specified NICE DCV GUI session.
--
-- 'status', 'createGUISessionAccessDetailsResponse_status' - The status of the operation.
--
-- 'httpStatus', 'createGUISessionAccessDetailsResponse_httpStatus' - The response's http status code.
newCreateGUISessionAccessDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGUISessionAccessDetailsResponse
newCreateGUISessionAccessDetailsResponse pHttpStatus_ =
  CreateGUISessionAccessDetailsResponse'
    { failureReason =
        Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      sessions = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The reason the operation failed.
createGUISessionAccessDetailsResponse_failureReason :: Lens.Lens' CreateGUISessionAccessDetailsResponse (Prelude.Maybe Prelude.Text)
createGUISessionAccessDetailsResponse_failureReason = Lens.lens (\CreateGUISessionAccessDetailsResponse' {failureReason} -> failureReason) (\s@CreateGUISessionAccessDetailsResponse' {} a -> s {failureReason = a} :: CreateGUISessionAccessDetailsResponse)

-- | The percentage of completion for the operation.
createGUISessionAccessDetailsResponse_percentageComplete :: Lens.Lens' CreateGUISessionAccessDetailsResponse (Prelude.Maybe Prelude.Int)
createGUISessionAccessDetailsResponse_percentageComplete = Lens.lens (\CreateGUISessionAccessDetailsResponse' {percentageComplete} -> percentageComplete) (\s@CreateGUISessionAccessDetailsResponse' {} a -> s {percentageComplete = a} :: CreateGUISessionAccessDetailsResponse)

-- | The resource name.
createGUISessionAccessDetailsResponse_resourceName :: Lens.Lens' CreateGUISessionAccessDetailsResponse (Prelude.Maybe Prelude.Text)
createGUISessionAccessDetailsResponse_resourceName = Lens.lens (\CreateGUISessionAccessDetailsResponse' {resourceName} -> resourceName) (\s@CreateGUISessionAccessDetailsResponse' {} a -> s {resourceName = a} :: CreateGUISessionAccessDetailsResponse)

-- | Returns information about the specified NICE DCV GUI session.
createGUISessionAccessDetailsResponse_sessions :: Lens.Lens' CreateGUISessionAccessDetailsResponse (Prelude.Maybe [Session])
createGUISessionAccessDetailsResponse_sessions = Lens.lens (\CreateGUISessionAccessDetailsResponse' {sessions} -> sessions) (\s@CreateGUISessionAccessDetailsResponse' {} a -> s {sessions = a} :: CreateGUISessionAccessDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the operation.
createGUISessionAccessDetailsResponse_status :: Lens.Lens' CreateGUISessionAccessDetailsResponse (Prelude.Maybe Status)
createGUISessionAccessDetailsResponse_status = Lens.lens (\CreateGUISessionAccessDetailsResponse' {status} -> status) (\s@CreateGUISessionAccessDetailsResponse' {} a -> s {status = a} :: CreateGUISessionAccessDetailsResponse)

-- | The response's http status code.
createGUISessionAccessDetailsResponse_httpStatus :: Lens.Lens' CreateGUISessionAccessDetailsResponse Prelude.Int
createGUISessionAccessDetailsResponse_httpStatus = Lens.lens (\CreateGUISessionAccessDetailsResponse' {httpStatus} -> httpStatus) (\s@CreateGUISessionAccessDetailsResponse' {} a -> s {httpStatus = a} :: CreateGUISessionAccessDetailsResponse)

instance
  Prelude.NFData
    CreateGUISessionAccessDetailsResponse
  where
  rnf CreateGUISessionAccessDetailsResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf percentageComplete
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf sessions
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
