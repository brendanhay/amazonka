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
-- Module      : Amazonka.Connect.DeleteVocabulary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the vocabulary that has the given identifier.
module Amazonka.Connect.DeleteVocabulary
  ( -- * Creating a Request
    DeleteVocabulary (..),
    newDeleteVocabulary,

    -- * Request Lenses
    deleteVocabulary_instanceId,
    deleteVocabulary_vocabularyId,

    -- * Destructuring the Response
    DeleteVocabularyResponse (..),
    newDeleteVocabularyResponse,

    -- * Response Lenses
    deleteVocabularyResponse_httpStatus,
    deleteVocabularyResponse_vocabularyArn,
    deleteVocabularyResponse_vocabularyId,
    deleteVocabularyResponse_state,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVocabulary' smart constructor.
data DeleteVocabulary = DeleteVocabulary'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the custom vocabulary.
    vocabularyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteVocabulary_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'vocabularyId', 'deleteVocabulary_vocabularyId' - The identifier of the custom vocabulary.
newDeleteVocabulary ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'vocabularyId'
  Prelude.Text ->
  DeleteVocabulary
newDeleteVocabulary pInstanceId_ pVocabularyId_ =
  DeleteVocabulary'
    { instanceId = pInstanceId_,
      vocabularyId = pVocabularyId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteVocabulary_instanceId :: Lens.Lens' DeleteVocabulary Prelude.Text
deleteVocabulary_instanceId = Lens.lens (\DeleteVocabulary' {instanceId} -> instanceId) (\s@DeleteVocabulary' {} a -> s {instanceId = a} :: DeleteVocabulary)

-- | The identifier of the custom vocabulary.
deleteVocabulary_vocabularyId :: Lens.Lens' DeleteVocabulary Prelude.Text
deleteVocabulary_vocabularyId = Lens.lens (\DeleteVocabulary' {vocabularyId} -> vocabularyId) (\s@DeleteVocabulary' {} a -> s {vocabularyId = a} :: DeleteVocabulary)

instance Core.AWSRequest DeleteVocabulary where
  type
    AWSResponse DeleteVocabulary =
      DeleteVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVocabularyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VocabularyArn")
            Prelude.<*> (x Data..:> "VocabularyId")
            Prelude.<*> (x Data..:> "State")
      )

instance Prelude.Hashable DeleteVocabulary where
  hashWithSalt _salt DeleteVocabulary' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` vocabularyId

instance Prelude.NFData DeleteVocabulary where
  rnf DeleteVocabulary' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf vocabularyId

instance Data.ToHeaders DeleteVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVocabulary where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteVocabulary where
  toPath DeleteVocabulary' {..} =
    Prelude.mconcat
      [ "/vocabulary-remove/",
        Data.toBS instanceId,
        "/",
        Data.toBS vocabularyId
      ]

instance Data.ToQuery DeleteVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVocabularyResponse' smart constructor.
data DeleteVocabularyResponse = DeleteVocabularyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the custom vocabulary.
    vocabularyArn :: Prelude.Text,
    -- | The identifier of the custom vocabulary.
    vocabularyId :: Prelude.Text,
    -- | The current state of the custom vocabulary.
    state :: VocabularyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVocabularyResponse_httpStatus' - The response's http status code.
--
-- 'vocabularyArn', 'deleteVocabularyResponse_vocabularyArn' - The Amazon Resource Name (ARN) of the custom vocabulary.
--
-- 'vocabularyId', 'deleteVocabularyResponse_vocabularyId' - The identifier of the custom vocabulary.
--
-- 'state', 'deleteVocabularyResponse_state' - The current state of the custom vocabulary.
newDeleteVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vocabularyArn'
  Prelude.Text ->
  -- | 'vocabularyId'
  Prelude.Text ->
  -- | 'state'
  VocabularyState ->
  DeleteVocabularyResponse
newDeleteVocabularyResponse
  pHttpStatus_
  pVocabularyArn_
  pVocabularyId_
  pState_ =
    DeleteVocabularyResponse'
      { httpStatus =
          pHttpStatus_,
        vocabularyArn = pVocabularyArn_,
        vocabularyId = pVocabularyId_,
        state = pState_
      }

-- | The response's http status code.
deleteVocabularyResponse_httpStatus :: Lens.Lens' DeleteVocabularyResponse Prelude.Int
deleteVocabularyResponse_httpStatus = Lens.lens (\DeleteVocabularyResponse' {httpStatus} -> httpStatus) (\s@DeleteVocabularyResponse' {} a -> s {httpStatus = a} :: DeleteVocabularyResponse)

-- | The Amazon Resource Name (ARN) of the custom vocabulary.
deleteVocabularyResponse_vocabularyArn :: Lens.Lens' DeleteVocabularyResponse Prelude.Text
deleteVocabularyResponse_vocabularyArn = Lens.lens (\DeleteVocabularyResponse' {vocabularyArn} -> vocabularyArn) (\s@DeleteVocabularyResponse' {} a -> s {vocabularyArn = a} :: DeleteVocabularyResponse)

-- | The identifier of the custom vocabulary.
deleteVocabularyResponse_vocabularyId :: Lens.Lens' DeleteVocabularyResponse Prelude.Text
deleteVocabularyResponse_vocabularyId = Lens.lens (\DeleteVocabularyResponse' {vocabularyId} -> vocabularyId) (\s@DeleteVocabularyResponse' {} a -> s {vocabularyId = a} :: DeleteVocabularyResponse)

-- | The current state of the custom vocabulary.
deleteVocabularyResponse_state :: Lens.Lens' DeleteVocabularyResponse VocabularyState
deleteVocabularyResponse_state = Lens.lens (\DeleteVocabularyResponse' {state} -> state) (\s@DeleteVocabularyResponse' {} a -> s {state = a} :: DeleteVocabularyResponse)

instance Prelude.NFData DeleteVocabularyResponse where
  rnf DeleteVocabularyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vocabularyArn
      `Prelude.seq` Prelude.rnf vocabularyId
      `Prelude.seq` Prelude.rnf state
