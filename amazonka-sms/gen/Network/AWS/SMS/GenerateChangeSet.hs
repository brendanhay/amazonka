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
-- Module      : Network.AWS.SMS.GenerateChangeSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a target change set for a currently launched stack and writes
-- it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Network.AWS.SMS.GenerateChangeSet
  ( -- * Creating a Request
    GenerateChangeSet (..),
    newGenerateChangeSet,

    -- * Request Lenses
    generateChangeSet_changesetFormat,
    generateChangeSet_appId,

    -- * Destructuring the Response
    GenerateChangeSetResponse (..),
    newGenerateChangeSetResponse,

    -- * Response Lenses
    generateChangeSetResponse_s3Location,
    generateChangeSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGenerateChangeSet' smart constructor.
data GenerateChangeSet = GenerateChangeSet'
  { -- | The format for the change set.
    changesetFormat :: Core.Maybe OutputFormat,
    -- | The ID of the application associated with the change set.
    appId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changesetFormat', 'generateChangeSet_changesetFormat' - The format for the change set.
--
-- 'appId', 'generateChangeSet_appId' - The ID of the application associated with the change set.
newGenerateChangeSet ::
  GenerateChangeSet
newGenerateChangeSet =
  GenerateChangeSet'
    { changesetFormat = Core.Nothing,
      appId = Core.Nothing
    }

-- | The format for the change set.
generateChangeSet_changesetFormat :: Lens.Lens' GenerateChangeSet (Core.Maybe OutputFormat)
generateChangeSet_changesetFormat = Lens.lens (\GenerateChangeSet' {changesetFormat} -> changesetFormat) (\s@GenerateChangeSet' {} a -> s {changesetFormat = a} :: GenerateChangeSet)

-- | The ID of the application associated with the change set.
generateChangeSet_appId :: Lens.Lens' GenerateChangeSet (Core.Maybe Core.Text)
generateChangeSet_appId = Lens.lens (\GenerateChangeSet' {appId} -> appId) (\s@GenerateChangeSet' {} a -> s {appId = a} :: GenerateChangeSet)

instance Core.AWSRequest GenerateChangeSet where
  type
    AWSResponse GenerateChangeSet =
      GenerateChangeSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateChangeSetResponse'
            Core.<$> (x Core..?> "s3Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GenerateChangeSet

instance Core.NFData GenerateChangeSet

instance Core.ToHeaders GenerateChangeSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GenerateChangeSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GenerateChangeSet where
  toJSON GenerateChangeSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("changesetFormat" Core..=)
              Core.<$> changesetFormat,
            ("appId" Core..=) Core.<$> appId
          ]
      )

instance Core.ToPath GenerateChangeSet where
  toPath = Core.const "/"

instance Core.ToQuery GenerateChangeSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGenerateChangeSetResponse' smart constructor.
data GenerateChangeSetResponse = GenerateChangeSetResponse'
  { -- | The location of the Amazon S3 object.
    s3Location :: Core.Maybe S3Location,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'generateChangeSetResponse_s3Location' - The location of the Amazon S3 object.
--
-- 'httpStatus', 'generateChangeSetResponse_httpStatus' - The response's http status code.
newGenerateChangeSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GenerateChangeSetResponse
newGenerateChangeSetResponse pHttpStatus_ =
  GenerateChangeSetResponse'
    { s3Location =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The location of the Amazon S3 object.
generateChangeSetResponse_s3Location :: Lens.Lens' GenerateChangeSetResponse (Core.Maybe S3Location)
generateChangeSetResponse_s3Location = Lens.lens (\GenerateChangeSetResponse' {s3Location} -> s3Location) (\s@GenerateChangeSetResponse' {} a -> s {s3Location = a} :: GenerateChangeSetResponse)

-- | The response's http status code.
generateChangeSetResponse_httpStatus :: Lens.Lens' GenerateChangeSetResponse Core.Int
generateChangeSetResponse_httpStatus = Lens.lens (\GenerateChangeSetResponse' {httpStatus} -> httpStatus) (\s@GenerateChangeSetResponse' {} a -> s {httpStatus = a} :: GenerateChangeSetResponse)

instance Core.NFData GenerateChangeSetResponse
