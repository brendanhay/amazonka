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
-- Associates the specified Amazon Web Services Systems Manager document
-- (SSM document) with the specified instances or targets.
--
-- When you associate a document with one or more instances using instance
-- IDs or tags, Amazon Web Services Systems Manager Agent (SSM Agent)
-- running on the instance processes the document and configures the
-- instance as specified.
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateAssociationBatch' smart constructor.
data CreateAssociationBatch = CreateAssociationBatch'
  { -- | One or more associations.
    entries :: Prelude.NonEmpty CreateAssociationBatchRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty CreateAssociationBatchRequestEntry ->
  CreateAssociationBatch
newCreateAssociationBatch pEntries_ =
  CreateAssociationBatch'
    { entries =
        Lens._Coerce Lens.# pEntries_
    }

-- | One or more associations.
createAssociationBatch_entries :: Lens.Lens' CreateAssociationBatch (Prelude.NonEmpty CreateAssociationBatchRequestEntry)
createAssociationBatch_entries = Lens.lens (\CreateAssociationBatch' {entries} -> entries) (\s@CreateAssociationBatch' {} a -> s {entries = a} :: CreateAssociationBatch) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateAssociationBatch where
  type
    AWSResponse CreateAssociationBatch =
      CreateAssociationBatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationBatchResponse'
            Prelude.<$> (x Core..?> "Successful" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Failed" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssociationBatch

instance Prelude.NFData CreateAssociationBatch

instance Core.ToHeaders CreateAssociationBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.CreateAssociationBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAssociationBatch where
  toJSON CreateAssociationBatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Entries" Core..= entries)]
      )

instance Core.ToPath CreateAssociationBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAssociationBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssociationBatchResponse' smart constructor.
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
  { -- | Information about the associations that succeeded.
    successful :: Prelude.Maybe [AssociationDescription],
    -- | Information about the associations that failed.
    failed :: Prelude.Maybe [FailedCreateAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateAssociationBatchResponse
newCreateAssociationBatchResponse pHttpStatus_ =
  CreateAssociationBatchResponse'
    { successful =
        Prelude.Nothing,
      failed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the associations that succeeded.
createAssociationBatchResponse_successful :: Lens.Lens' CreateAssociationBatchResponse (Prelude.Maybe [AssociationDescription])
createAssociationBatchResponse_successful = Lens.lens (\CreateAssociationBatchResponse' {successful} -> successful) (\s@CreateAssociationBatchResponse' {} a -> s {successful = a} :: CreateAssociationBatchResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the associations that failed.
createAssociationBatchResponse_failed :: Lens.Lens' CreateAssociationBatchResponse (Prelude.Maybe [FailedCreateAssociation])
createAssociationBatchResponse_failed = Lens.lens (\CreateAssociationBatchResponse' {failed} -> failed) (\s@CreateAssociationBatchResponse' {} a -> s {failed = a} :: CreateAssociationBatchResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createAssociationBatchResponse_httpStatus :: Lens.Lens' CreateAssociationBatchResponse Prelude.Int
createAssociationBatchResponse_httpStatus = Lens.lens (\CreateAssociationBatchResponse' {httpStatus} -> httpStatus) (\s@CreateAssociationBatchResponse' {} a -> s {httpStatus = a} :: CreateAssociationBatchResponse)

instance
  Prelude.NFData
    CreateAssociationBatchResponse
