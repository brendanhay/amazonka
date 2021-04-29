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
-- Module      : Network.AWS.DirectoryService.CancelSchemaExtension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-progress schema extension to a Microsoft AD directory.
-- Once a schema extension has started replicating to all domain
-- controllers, the task can no longer be canceled. A schema extension can
-- be canceled during any of the following states; @Initializing@,
-- @CreatingSnapshot@, and @UpdatingSchema@.
module Network.AWS.DirectoryService.CancelSchemaExtension
  ( -- * Creating a Request
    CancelSchemaExtension (..),
    newCancelSchemaExtension,

    -- * Request Lenses
    cancelSchemaExtension_directoryId,
    cancelSchemaExtension_schemaExtensionId,

    -- * Destructuring the Response
    CancelSchemaExtensionResponse (..),
    newCancelSchemaExtensionResponse,

    -- * Response Lenses
    cancelSchemaExtensionResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelSchemaExtension' smart constructor.
data CancelSchemaExtension = CancelSchemaExtension'
  { -- | The identifier of the directory whose schema extension will be canceled.
    directoryId :: Prelude.Text,
    -- | The identifier of the schema extension that will be canceled.
    schemaExtensionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelSchemaExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'cancelSchemaExtension_directoryId' - The identifier of the directory whose schema extension will be canceled.
--
-- 'schemaExtensionId', 'cancelSchemaExtension_schemaExtensionId' - The identifier of the schema extension that will be canceled.
newCancelSchemaExtension ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'schemaExtensionId'
  Prelude.Text ->
  CancelSchemaExtension
newCancelSchemaExtension
  pDirectoryId_
  pSchemaExtensionId_ =
    CancelSchemaExtension'
      { directoryId = pDirectoryId_,
        schemaExtensionId = pSchemaExtensionId_
      }

-- | The identifier of the directory whose schema extension will be canceled.
cancelSchemaExtension_directoryId :: Lens.Lens' CancelSchemaExtension Prelude.Text
cancelSchemaExtension_directoryId = Lens.lens (\CancelSchemaExtension' {directoryId} -> directoryId) (\s@CancelSchemaExtension' {} a -> s {directoryId = a} :: CancelSchemaExtension)

-- | The identifier of the schema extension that will be canceled.
cancelSchemaExtension_schemaExtensionId :: Lens.Lens' CancelSchemaExtension Prelude.Text
cancelSchemaExtension_schemaExtensionId = Lens.lens (\CancelSchemaExtension' {schemaExtensionId} -> schemaExtensionId) (\s@CancelSchemaExtension' {} a -> s {schemaExtensionId = a} :: CancelSchemaExtension)

instance Prelude.AWSRequest CancelSchemaExtension where
  type
    Rs CancelSchemaExtension =
      CancelSchemaExtensionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelSchemaExtensionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSchemaExtension

instance Prelude.NFData CancelSchemaExtension

instance Prelude.ToHeaders CancelSchemaExtension where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.CancelSchemaExtension" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelSchemaExtension where
  toJSON CancelSchemaExtension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just
              ("SchemaExtensionId" Prelude..= schemaExtensionId)
          ]
      )

instance Prelude.ToPath CancelSchemaExtension where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelSchemaExtension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelSchemaExtensionResponse' smart constructor.
data CancelSchemaExtensionResponse = CancelSchemaExtensionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelSchemaExtensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelSchemaExtensionResponse_httpStatus' - The response's http status code.
newCancelSchemaExtensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelSchemaExtensionResponse
newCancelSchemaExtensionResponse pHttpStatus_ =
  CancelSchemaExtensionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelSchemaExtensionResponse_httpStatus :: Lens.Lens' CancelSchemaExtensionResponse Prelude.Int
cancelSchemaExtensionResponse_httpStatus = Lens.lens (\CancelSchemaExtensionResponse' {httpStatus} -> httpStatus) (\s@CancelSchemaExtensionResponse' {} a -> s {httpStatus = a} :: CancelSchemaExtensionResponse)

instance Prelude.NFData CancelSchemaExtensionResponse
