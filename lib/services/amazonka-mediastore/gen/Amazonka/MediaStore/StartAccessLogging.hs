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
-- Module      : Amazonka.MediaStore.StartAccessLogging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts access logging on the specified container. When you enable access
-- logging on a container, MediaStore delivers access logs for objects
-- stored in that container to Amazon CloudWatch Logs.
module Amazonka.MediaStore.StartAccessLogging
  ( -- * Creating a Request
    StartAccessLogging (..),
    newStartAccessLogging,

    -- * Request Lenses
    startAccessLogging_containerName,

    -- * Destructuring the Response
    StartAccessLoggingResponse (..),
    newStartAccessLoggingResponse,

    -- * Response Lenses
    startAccessLoggingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAccessLogging' smart constructor.
data StartAccessLogging = StartAccessLogging'
  { -- | The name of the container that you want to start access logging on.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAccessLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'startAccessLogging_containerName' - The name of the container that you want to start access logging on.
newStartAccessLogging ::
  -- | 'containerName'
  Prelude.Text ->
  StartAccessLogging
newStartAccessLogging pContainerName_ =
  StartAccessLogging'
    { containerName =
        pContainerName_
    }

-- | The name of the container that you want to start access logging on.
startAccessLogging_containerName :: Lens.Lens' StartAccessLogging Prelude.Text
startAccessLogging_containerName = Lens.lens (\StartAccessLogging' {containerName} -> containerName) (\s@StartAccessLogging' {} a -> s {containerName = a} :: StartAccessLogging)

instance Core.AWSRequest StartAccessLogging where
  type
    AWSResponse StartAccessLogging =
      StartAccessLoggingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAccessLoggingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAccessLogging where
  hashWithSalt _salt StartAccessLogging' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData StartAccessLogging where
  rnf StartAccessLogging' {..} =
    Prelude.rnf containerName

instance Data.ToHeaders StartAccessLogging where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.StartAccessLogging" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAccessLogging where
  toJSON StartAccessLogging' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath StartAccessLogging where
  toPath = Prelude.const "/"

instance Data.ToQuery StartAccessLogging where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAccessLoggingResponse' smart constructor.
data StartAccessLoggingResponse = StartAccessLoggingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAccessLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startAccessLoggingResponse_httpStatus' - The response's http status code.
newStartAccessLoggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartAccessLoggingResponse
newStartAccessLoggingResponse pHttpStatus_ =
  StartAccessLoggingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startAccessLoggingResponse_httpStatus :: Lens.Lens' StartAccessLoggingResponse Prelude.Int
startAccessLoggingResponse_httpStatus = Lens.lens (\StartAccessLoggingResponse' {httpStatus} -> httpStatus) (\s@StartAccessLoggingResponse' {} a -> s {httpStatus = a} :: StartAccessLoggingResponse)

instance Prelude.NFData StartAccessLoggingResponse where
  rnf StartAccessLoggingResponse' {..} =
    Prelude.rnf httpStatus
