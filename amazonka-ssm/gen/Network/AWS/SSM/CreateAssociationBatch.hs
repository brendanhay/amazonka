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
-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Systems Manager document with the specified
-- instances or targets.
--
-- When you associate a document with one or more instances using instance
-- IDs or tags, SSM Agent running on the instance processes the document
-- and configures the instance as specified.
--
-- If you associate a document with an instance that already has an
-- associated document, the system returns the AssociationAlreadyExists
-- exception.
module Network.AWS.SSM.CreateAssociationBatch
  ( -- * Creating a Request
    CreateAssociationBatch (..),
    newCreateAssociationBatch,

    -- * Request Lenses
    createAssociationBatch_entries,

    -- * Destructuring the Response
    CreateAssociationBatchResponse (..),
    newCreateAssociationBatchResponse,

    -- * Response Lenses
    createAssociationBatchResponse_successful,
    createAssociationBatchResponse_failed,
    createAssociationBatchResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateAssociationBatch' smart constructor.
data CreateAssociationBatch = CreateAssociationBatch'
  { -- | One or more associations.
    entries :: Core.NonEmpty CreateAssociationBatchRequestEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAssociationBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'createAssociationBatch_entries' - One or more associations.
newCreateAssociationBatch ::
  -- | 'entries'
  Core.NonEmpty CreateAssociationBatchRequestEntry ->
  CreateAssociationBatch
newCreateAssociationBatch pEntries_ =
  CreateAssociationBatch'
    { entries =
        Lens._Coerce Lens.# pEntries_
    }

-- | One or more associations.
createAssociationBatch_entries :: Lens.Lens' CreateAssociationBatch (Core.NonEmpty CreateAssociationBatchRequestEntry)
createAssociationBatch_entries = Lens.lens (\CreateAssociationBatch' {entries} -> entries) (\s@CreateAssociationBatch' {} a -> s {entries = a} :: CreateAssociationBatch) Core.. Lens._Coerce

instance Core.AWSRequest CreateAssociationBatch where
  type
    AWSResponse CreateAssociationBatch =
      CreateAssociationBatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationBatchResponse'
            Core.<$> (x Core..?> "Successful" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Failed" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateAssociationBatch

instance Core.NFData CreateAssociationBatch

instance Core.ToHeaders CreateAssociationBatch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.CreateAssociationBatch" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateAssociationBatch where
  toJSON CreateAssociationBatch' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Entries" Core..= entries)]
      )

instance Core.ToPath CreateAssociationBatch where
  toPath = Core.const "/"

instance Core.ToQuery CreateAssociationBatch where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAssociationBatchResponse' smart constructor.
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
  { -- | Information about the associations that succeeded.
    successful :: Core.Maybe [AssociationDescription],
    -- | Information about the associations that failed.
    failed :: Core.Maybe [FailedCreateAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAssociationBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'createAssociationBatchResponse_successful' - Information about the associations that succeeded.
--
-- 'failed', 'createAssociationBatchResponse_failed' - Information about the associations that failed.
--
-- 'httpStatus', 'createAssociationBatchResponse_httpStatus' - The response's http status code.
newCreateAssociationBatchResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateAssociationBatchResponse
newCreateAssociationBatchResponse pHttpStatus_ =
  CreateAssociationBatchResponse'
    { successful =
        Core.Nothing,
      failed = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the associations that succeeded.
createAssociationBatchResponse_successful :: Lens.Lens' CreateAssociationBatchResponse (Core.Maybe [AssociationDescription])
createAssociationBatchResponse_successful = Lens.lens (\CreateAssociationBatchResponse' {successful} -> successful) (\s@CreateAssociationBatchResponse' {} a -> s {successful = a} :: CreateAssociationBatchResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the associations that failed.
createAssociationBatchResponse_failed :: Lens.Lens' CreateAssociationBatchResponse (Core.Maybe [FailedCreateAssociation])
createAssociationBatchResponse_failed = Lens.lens (\CreateAssociationBatchResponse' {failed} -> failed) (\s@CreateAssociationBatchResponse' {} a -> s {failed = a} :: CreateAssociationBatchResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createAssociationBatchResponse_httpStatus :: Lens.Lens' CreateAssociationBatchResponse Core.Int
createAssociationBatchResponse_httpStatus = Lens.lens (\CreateAssociationBatchResponse' {httpStatus} -> httpStatus) (\s@CreateAssociationBatchResponse' {} a -> s {httpStatus = a} :: CreateAssociationBatchResponse)

instance Core.NFData CreateAssociationBatchResponse
