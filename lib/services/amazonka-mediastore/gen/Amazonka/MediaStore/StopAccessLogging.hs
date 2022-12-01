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
-- Module      : Amazonka.MediaStore.StopAccessLogging
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops access logging on the specified container. When you stop access
-- logging on a container, MediaStore stops sending access logs to Amazon
-- CloudWatch Logs. These access logs are not saved and are not
-- retrievable.
module Amazonka.MediaStore.StopAccessLogging
  ( -- * Creating a Request
    StopAccessLogging (..),
    newStopAccessLogging,

    -- * Request Lenses
    stopAccessLogging_containerName,

    -- * Destructuring the Response
    StopAccessLoggingResponse (..),
    newStopAccessLoggingResponse,

    -- * Response Lenses
    stopAccessLoggingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopAccessLogging' smart constructor.
data StopAccessLogging = StopAccessLogging'
  { -- | The name of the container that you want to stop access logging on.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAccessLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'stopAccessLogging_containerName' - The name of the container that you want to stop access logging on.
newStopAccessLogging ::
  -- | 'containerName'
  Prelude.Text ->
  StopAccessLogging
newStopAccessLogging pContainerName_ =
  StopAccessLogging' {containerName = pContainerName_}

-- | The name of the container that you want to stop access logging on.
stopAccessLogging_containerName :: Lens.Lens' StopAccessLogging Prelude.Text
stopAccessLogging_containerName = Lens.lens (\StopAccessLogging' {containerName} -> containerName) (\s@StopAccessLogging' {} a -> s {containerName = a} :: StopAccessLogging)

instance Core.AWSRequest StopAccessLogging where
  type
    AWSResponse StopAccessLogging =
      StopAccessLoggingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAccessLoggingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopAccessLogging where
  hashWithSalt _salt StopAccessLogging' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData StopAccessLogging where
  rnf StopAccessLogging' {..} =
    Prelude.rnf containerName

instance Core.ToHeaders StopAccessLogging where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.StopAccessLogging" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopAccessLogging where
  toJSON StopAccessLogging' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Core..= containerName)
          ]
      )

instance Core.ToPath StopAccessLogging where
  toPath = Prelude.const "/"

instance Core.ToQuery StopAccessLogging where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAccessLoggingResponse' smart constructor.
data StopAccessLoggingResponse = StopAccessLoggingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAccessLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopAccessLoggingResponse_httpStatus' - The response's http status code.
newStopAccessLoggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopAccessLoggingResponse
newStopAccessLoggingResponse pHttpStatus_ =
  StopAccessLoggingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopAccessLoggingResponse_httpStatus :: Lens.Lens' StopAccessLoggingResponse Prelude.Int
stopAccessLoggingResponse_httpStatus = Lens.lens (\StopAccessLoggingResponse' {httpStatus} -> httpStatus) (\s@StopAccessLoggingResponse' {} a -> s {httpStatus = a} :: StopAccessLoggingResponse)

instance Prelude.NFData StopAccessLoggingResponse where
  rnf StopAccessLoggingResponse' {..} =
    Prelude.rnf httpStatus
