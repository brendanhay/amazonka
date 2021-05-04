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
-- Module      : Network.AWS.MediaStore.StartAccessLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts access logging on the specified container. When you enable access
-- logging on a container, MediaStore delivers access logs for objects
-- stored in that container to Amazon CloudWatch Logs.
module Network.AWS.MediaStore.StartAccessLogging
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartAccessLogging' smart constructor.
data StartAccessLogging = StartAccessLogging'
  { -- | The name of the container that you want to start access logging on.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StartAccessLogging where
  type
    Rs StartAccessLogging =
      StartAccessLoggingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAccessLoggingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAccessLogging

instance Prelude.NFData StartAccessLogging

instance Prelude.ToHeaders StartAccessLogging where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MediaStore_20170901.StartAccessLogging" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartAccessLogging where
  toJSON StartAccessLogging' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Prelude..= containerName)
          ]
      )

instance Prelude.ToPath StartAccessLogging where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartAccessLogging where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAccessLoggingResponse' smart constructor.
data StartAccessLoggingResponse = StartAccessLoggingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StartAccessLoggingResponse
