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
-- Module      : Network.AWS.Rekognition.StartStreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts processing a stream processor. You create a stream processor by
-- calling CreateStreamProcessor. To tell @StartStreamProcessor@ which
-- stream processor to start, use the value of the @Name@ field specified
-- in the call to @CreateStreamProcessor@.
module Network.AWS.Rekognition.StartStreamProcessor
  ( -- * Creating a Request
    StartStreamProcessor (..),
    newStartStreamProcessor,

    -- * Request Lenses
    startStreamProcessor_name,

    -- * Destructuring the Response
    StartStreamProcessorResponse (..),
    newStartStreamProcessorResponse,

    -- * Response Lenses
    startStreamProcessorResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartStreamProcessor' smart constructor.
data StartStreamProcessor = StartStreamProcessor'
  { -- | The name of the stream processor to start processing.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startStreamProcessor_name' - The name of the stream processor to start processing.
newStartStreamProcessor ::
  -- | 'name'
  Prelude.Text ->
  StartStreamProcessor
newStartStreamProcessor pName_ =
  StartStreamProcessor' {name = pName_}

-- | The name of the stream processor to start processing.
startStreamProcessor_name :: Lens.Lens' StartStreamProcessor Prelude.Text
startStreamProcessor_name = Lens.lens (\StartStreamProcessor' {name} -> name) (\s@StartStreamProcessor' {} a -> s {name = a} :: StartStreamProcessor)

instance Prelude.AWSRequest StartStreamProcessor where
  type
    Rs StartStreamProcessor =
      StartStreamProcessorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartStreamProcessorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartStreamProcessor

instance Prelude.NFData StartStreamProcessor

instance Prelude.ToHeaders StartStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.StartStreamProcessor" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartStreamProcessor where
  toJSON StartStreamProcessor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StartStreamProcessor where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStreamProcessorResponse' smart constructor.
data StartStreamProcessorResponse = StartStreamProcessorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startStreamProcessorResponse_httpStatus' - The response's http status code.
newStartStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartStreamProcessorResponse
newStartStreamProcessorResponse pHttpStatus_ =
  StartStreamProcessorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startStreamProcessorResponse_httpStatus :: Lens.Lens' StartStreamProcessorResponse Prelude.Int
startStreamProcessorResponse_httpStatus = Lens.lens (\StartStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@StartStreamProcessorResponse' {} a -> s {httpStatus = a} :: StartStreamProcessorResponse)

instance Prelude.NFData StartStreamProcessorResponse
