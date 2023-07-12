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
-- Module      : Amazonka.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Amazon Web Services Systems Manager document
-- (SSM document) with the specified managed nodes or targets.
--
-- When you associate a document with one or more managed nodes using IDs
-- or tags, Amazon Web Services Systems Manager Agent (SSM Agent) running
-- on the managed node processes the document and configures the node as
-- specified.
--
-- If you associate a document with a managed node that already has an
-- associated document, the system returns the AssociationAlreadyExists
-- exception.
module Amazonka.SSM.CreateAssociationBatch
  ( -- * Creating a Request
    CreateAssociationBatch (..),
    newCreateAssociationBatch,

    -- * Request Lenses
    createAssociationBatch_entries,

    -- * Destructuring the Response
    CreateAssociationBatchResponse (..),
    newCreateAssociationBatchResponse,

    -- * Response Lenses
    createAssociationBatchResponse_failed,
    createAssociationBatchResponse_successful,
    createAssociationBatchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreateAssociationBatch' smart constructor.
data CreateAssociationBatch = CreateAssociationBatch'
  { -- | One or more associations.
    entries :: Prelude.NonEmpty CreateAssociationBatchRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
        Lens.coerced Lens.# pEntries_
    }

-- | One or more associations.
createAssociationBatch_entries :: Lens.Lens' CreateAssociationBatch (Prelude.NonEmpty CreateAssociationBatchRequestEntry)
createAssociationBatch_entries = Lens.lens (\CreateAssociationBatch' {entries} -> entries) (\s@CreateAssociationBatch' {} a -> s {entries = a} :: CreateAssociationBatch) Prelude.. Lens.coerced

instance Core.AWSRequest CreateAssociationBatch where
  type
    AWSResponse CreateAssociationBatch =
      CreateAssociationBatchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationBatchResponse'
            Prelude.<$> (x Data..?> "Failed" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Successful" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssociationBatch where
  hashWithSalt _salt CreateAssociationBatch' {..} =
    _salt `Prelude.hashWithSalt` entries

instance Prelude.NFData CreateAssociationBatch where
  rnf CreateAssociationBatch' {..} = Prelude.rnf entries

instance Data.ToHeaders CreateAssociationBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.CreateAssociationBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssociationBatch where
  toJSON CreateAssociationBatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Entries" Data..= entries)]
      )

instance Data.ToPath CreateAssociationBatch where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAssociationBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssociationBatchResponse' smart constructor.
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
  { -- | Information about the associations that failed.
    failed :: Prelude.Maybe [FailedCreateAssociation],
    -- | Information about the associations that succeeded.
    successful :: Prelude.Maybe [AssociationDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssociationBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'createAssociationBatchResponse_failed' - Information about the associations that failed.
--
-- 'successful', 'createAssociationBatchResponse_successful' - Information about the associations that succeeded.
--
-- 'httpStatus', 'createAssociationBatchResponse_httpStatus' - The response's http status code.
newCreateAssociationBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssociationBatchResponse
newCreateAssociationBatchResponse pHttpStatus_ =
  CreateAssociationBatchResponse'
    { failed =
        Prelude.Nothing,
      successful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the associations that failed.
createAssociationBatchResponse_failed :: Lens.Lens' CreateAssociationBatchResponse (Prelude.Maybe [FailedCreateAssociation])
createAssociationBatchResponse_failed = Lens.lens (\CreateAssociationBatchResponse' {failed} -> failed) (\s@CreateAssociationBatchResponse' {} a -> s {failed = a} :: CreateAssociationBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the associations that succeeded.
createAssociationBatchResponse_successful :: Lens.Lens' CreateAssociationBatchResponse (Prelude.Maybe [AssociationDescription])
createAssociationBatchResponse_successful = Lens.lens (\CreateAssociationBatchResponse' {successful} -> successful) (\s@CreateAssociationBatchResponse' {} a -> s {successful = a} :: CreateAssociationBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createAssociationBatchResponse_httpStatus :: Lens.Lens' CreateAssociationBatchResponse Prelude.Int
createAssociationBatchResponse_httpStatus = Lens.lens (\CreateAssociationBatchResponse' {httpStatus} -> httpStatus) (\s@CreateAssociationBatchResponse' {} a -> s {httpStatus = a} :: CreateAssociationBatchResponse)

instance
  Prelude.NFData
    CreateAssociationBatchResponse
  where
  rnf CreateAssociationBatchResponse' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf successful
      `Prelude.seq` Prelude.rnf httpStatus
