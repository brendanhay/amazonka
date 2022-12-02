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
-- Module      : Amazonka.Rekognition.StopStreamProcessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running stream processor that was created by
-- CreateStreamProcessor.
module Amazonka.Rekognition.StopStreamProcessor
  ( -- * Creating a Request
    StopStreamProcessor (..),
    newStopStreamProcessor,

    -- * Request Lenses
    stopStreamProcessor_name,

    -- * Destructuring the Response
    StopStreamProcessorResponse (..),
    newStopStreamProcessorResponse,

    -- * Response Lenses
    stopStreamProcessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopStreamProcessor' smart constructor.
data StopStreamProcessor = StopStreamProcessor'
  { -- | The name of a stream processor created by CreateStreamProcessor.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopStreamProcessor_name' - The name of a stream processor created by CreateStreamProcessor.
newStopStreamProcessor ::
  -- | 'name'
  Prelude.Text ->
  StopStreamProcessor
newStopStreamProcessor pName_ =
  StopStreamProcessor' {name = pName_}

-- | The name of a stream processor created by CreateStreamProcessor.
stopStreamProcessor_name :: Lens.Lens' StopStreamProcessor Prelude.Text
stopStreamProcessor_name = Lens.lens (\StopStreamProcessor' {name} -> name) (\s@StopStreamProcessor' {} a -> s {name = a} :: StopStreamProcessor)

instance Core.AWSRequest StopStreamProcessor where
  type
    AWSResponse StopStreamProcessor =
      StopStreamProcessorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopStreamProcessorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopStreamProcessor where
  hashWithSalt _salt StopStreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StopStreamProcessor where
  rnf StopStreamProcessor' {..} = Prelude.rnf name

instance Data.ToHeaders StopStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StopStreamProcessor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopStreamProcessor where
  toJSON StopStreamProcessor' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath StopStreamProcessor where
  toPath = Prelude.const "/"

instance Data.ToQuery StopStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopStreamProcessorResponse' smart constructor.
data StopStreamProcessorResponse = StopStreamProcessorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopStreamProcessorResponse_httpStatus' - The response's http status code.
newStopStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopStreamProcessorResponse
newStopStreamProcessorResponse pHttpStatus_ =
  StopStreamProcessorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopStreamProcessorResponse_httpStatus :: Lens.Lens' StopStreamProcessorResponse Prelude.Int
stopStreamProcessorResponse_httpStatus = Lens.lens (\StopStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@StopStreamProcessorResponse' {} a -> s {httpStatus = a} :: StopStreamProcessorResponse)

instance Prelude.NFData StopStreamProcessorResponse where
  rnf StopStreamProcessorResponse' {..} =
    Prelude.rnf httpStatus
