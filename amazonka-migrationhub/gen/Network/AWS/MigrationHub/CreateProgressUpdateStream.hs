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
-- Module      : Network.AWS.MigrationHub.CreateProgressUpdateStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a progress update stream which is an AWS resource used for
-- access control as well as a namespace for migration task names that is
-- implicitly linked to your AWS account. It must uniquely identify the
-- migration tool as it is used for all updates made by the tool; however,
-- it does not need to be unique for each AWS account because it is scoped
-- to the AWS account.
module Network.AWS.MigrationHub.CreateProgressUpdateStream
  ( -- * Creating a Request
    CreateProgressUpdateStream (..),
    newCreateProgressUpdateStream,

    -- * Request Lenses
    createProgressUpdateStream_dryRun,
    createProgressUpdateStream_progressUpdateStreamName,

    -- * Destructuring the Response
    CreateProgressUpdateStreamResponse (..),
    newCreateProgressUpdateStreamResponse,

    -- * Response Lenses
    createProgressUpdateStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProgressUpdateStream' smart constructor.
data CreateProgressUpdateStream = CreateProgressUpdateStream'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream. /Do not store personal data in
    -- this field./
    progressUpdateStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProgressUpdateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createProgressUpdateStream_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStreamName', 'createProgressUpdateStream_progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
newCreateProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Prelude.Text ->
  CreateProgressUpdateStream
newCreateProgressUpdateStream
  pProgressUpdateStreamName_ =
    CreateProgressUpdateStream'
      { dryRun =
          Prelude.Nothing,
        progressUpdateStreamName =
          pProgressUpdateStreamName_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
createProgressUpdateStream_dryRun :: Lens.Lens' CreateProgressUpdateStream (Prelude.Maybe Prelude.Bool)
createProgressUpdateStream_dryRun = Lens.lens (\CreateProgressUpdateStream' {dryRun} -> dryRun) (\s@CreateProgressUpdateStream' {} a -> s {dryRun = a} :: CreateProgressUpdateStream)

-- | The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
createProgressUpdateStream_progressUpdateStreamName :: Lens.Lens' CreateProgressUpdateStream Prelude.Text
createProgressUpdateStream_progressUpdateStreamName = Lens.lens (\CreateProgressUpdateStream' {progressUpdateStreamName} -> progressUpdateStreamName) (\s@CreateProgressUpdateStream' {} a -> s {progressUpdateStreamName = a} :: CreateProgressUpdateStream)

instance
  Prelude.AWSRequest
    CreateProgressUpdateStream
  where
  type
    Rs CreateProgressUpdateStream =
      CreateProgressUpdateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateProgressUpdateStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProgressUpdateStream

instance Prelude.NFData CreateProgressUpdateStream

instance Prelude.ToHeaders CreateProgressUpdateStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.CreateProgressUpdateStream" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProgressUpdateStream where
  toJSON CreateProgressUpdateStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DryRun" Prelude..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStreamName"
                  Prelude..= progressUpdateStreamName
              )
          ]
      )

instance Prelude.ToPath CreateProgressUpdateStream where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProgressUpdateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProgressUpdateStreamResponse' smart constructor.
data CreateProgressUpdateStreamResponse = CreateProgressUpdateStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProgressUpdateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProgressUpdateStreamResponse_httpStatus' - The response's http status code.
newCreateProgressUpdateStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProgressUpdateStreamResponse
newCreateProgressUpdateStreamResponse pHttpStatus_ =
  CreateProgressUpdateStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createProgressUpdateStreamResponse_httpStatus :: Lens.Lens' CreateProgressUpdateStreamResponse Prelude.Int
createProgressUpdateStreamResponse_httpStatus = Lens.lens (\CreateProgressUpdateStreamResponse' {httpStatus} -> httpStatus) (\s@CreateProgressUpdateStreamResponse' {} a -> s {httpStatus = a} :: CreateProgressUpdateStreamResponse)

instance
  Prelude.NFData
    CreateProgressUpdateStreamResponse
