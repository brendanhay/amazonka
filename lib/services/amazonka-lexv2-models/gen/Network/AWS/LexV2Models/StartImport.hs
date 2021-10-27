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
-- Module      : Network.AWS.LexV2Models.StartImport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts importing a bot or bot locale from a zip archive that you
-- uploaded to an S3 bucket.
module Network.AWS.LexV2Models.StartImport
  ( -- * Creating a Request
    StartImport (..),
    newStartImport,

    -- * Request Lenses
    startImport_filePassword,
    startImport_importId,
    startImport_resourceSpecification,
    startImport_mergeStrategy,

    -- * Destructuring the Response
    StartImportResponse (..),
    newStartImportResponse,

    -- * Response Lenses
    startImportResponse_resourceSpecification,
    startImportResponse_importId,
    startImportResponse_creationDateTime,
    startImportResponse_mergeStrategy,
    startImportResponse_importStatus,
    startImportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartImport' smart constructor.
data StartImport = StartImport'
  { -- | The password used to encrypt the zip archive that contains the bot or
    -- bot locale definition. You should always encrypt the zip archive to
    -- protect it during transit between your site and Amazon Lex.
    filePassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The unique identifier for the import. It is included in the response
    -- from the operation.
    importId :: Prelude.Text,
    -- | Parameters for creating the bot or bot locale.
    resourceSpecification :: ImportResourceSpecification,
    -- | The strategy to use when there is a name conflict between the imported
    -- resource and an existing resource. When the merge strategy is
    -- @FailOnConflict@ existing resources are not overwritten and the import
    -- fails.
    mergeStrategy :: MergeStrategy
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePassword', 'startImport_filePassword' - The password used to encrypt the zip archive that contains the bot or
-- bot locale definition. You should always encrypt the zip archive to
-- protect it during transit between your site and Amazon Lex.
--
-- 'importId', 'startImport_importId' - The unique identifier for the import. It is included in the response
-- from the operation.
--
-- 'resourceSpecification', 'startImport_resourceSpecification' - Parameters for creating the bot or bot locale.
--
-- 'mergeStrategy', 'startImport_mergeStrategy' - The strategy to use when there is a name conflict between the imported
-- resource and an existing resource. When the merge strategy is
-- @FailOnConflict@ existing resources are not overwritten and the import
-- fails.
newStartImport ::
  -- | 'importId'
  Prelude.Text ->
  -- | 'resourceSpecification'
  ImportResourceSpecification ->
  -- | 'mergeStrategy'
  MergeStrategy ->
  StartImport
newStartImport
  pImportId_
  pResourceSpecification_
  pMergeStrategy_ =
    StartImport'
      { filePassword = Prelude.Nothing,
        importId = pImportId_,
        resourceSpecification = pResourceSpecification_,
        mergeStrategy = pMergeStrategy_
      }

-- | The password used to encrypt the zip archive that contains the bot or
-- bot locale definition. You should always encrypt the zip archive to
-- protect it during transit between your site and Amazon Lex.
startImport_filePassword :: Lens.Lens' StartImport (Prelude.Maybe Prelude.Text)
startImport_filePassword = Lens.lens (\StartImport' {filePassword} -> filePassword) (\s@StartImport' {} a -> s {filePassword = a} :: StartImport) Prelude.. Lens.mapping Core._Sensitive

-- | The unique identifier for the import. It is included in the response
-- from the operation.
startImport_importId :: Lens.Lens' StartImport Prelude.Text
startImport_importId = Lens.lens (\StartImport' {importId} -> importId) (\s@StartImport' {} a -> s {importId = a} :: StartImport)

-- | Parameters for creating the bot or bot locale.
startImport_resourceSpecification :: Lens.Lens' StartImport ImportResourceSpecification
startImport_resourceSpecification = Lens.lens (\StartImport' {resourceSpecification} -> resourceSpecification) (\s@StartImport' {} a -> s {resourceSpecification = a} :: StartImport)

-- | The strategy to use when there is a name conflict between the imported
-- resource and an existing resource. When the merge strategy is
-- @FailOnConflict@ existing resources are not overwritten and the import
-- fails.
startImport_mergeStrategy :: Lens.Lens' StartImport MergeStrategy
startImport_mergeStrategy = Lens.lens (\StartImport' {mergeStrategy} -> mergeStrategy) (\s@StartImport' {} a -> s {mergeStrategy = a} :: StartImport)

instance Core.AWSRequest StartImport where
  type AWSResponse StartImport = StartImportResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportResponse'
            Prelude.<$> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "importId")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "mergeStrategy")
            Prelude.<*> (x Core..?> "importStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImport

instance Prelude.NFData StartImport

instance Core.ToHeaders StartImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartImport where
  toJSON StartImport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filePassword" Core..=) Prelude.<$> filePassword,
            Prelude.Just ("importId" Core..= importId),
            Prelude.Just
              ( "resourceSpecification"
                  Core..= resourceSpecification
              ),
            Prelude.Just
              ("mergeStrategy" Core..= mergeStrategy)
          ]
      )

instance Core.ToPath StartImport where
  toPath = Prelude.const "/imports/"

instance Core.ToQuery StartImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImportResponse' smart constructor.
data StartImportResponse = StartImportResponse'
  { -- | The parameters used when importing the bot or bot locale.
    resourceSpecification :: Prelude.Maybe ImportResourceSpecification,
    -- | A unique identifier for the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the import request was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The strategy used when there was a name conflict between the imported
    -- resource and an existing resource. When the merge strategy is
    -- @FailOnConflict@ existing resources are not overwritten and the import
    -- fails.
    mergeStrategy :: Prelude.Maybe MergeStrategy,
    -- | The current status of the import. When the status is @Complete@ the bot
    -- or bot alias is ready to use.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpecification', 'startImportResponse_resourceSpecification' - The parameters used when importing the bot or bot locale.
--
-- 'importId', 'startImportResponse_importId' - A unique identifier for the import.
--
-- 'creationDateTime', 'startImportResponse_creationDateTime' - The date and time that the import request was created.
--
-- 'mergeStrategy', 'startImportResponse_mergeStrategy' - The strategy used when there was a name conflict between the imported
-- resource and an existing resource. When the merge strategy is
-- @FailOnConflict@ existing resources are not overwritten and the import
-- fails.
--
-- 'importStatus', 'startImportResponse_importStatus' - The current status of the import. When the status is @Complete@ the bot
-- or bot alias is ready to use.
--
-- 'httpStatus', 'startImportResponse_httpStatus' - The response's http status code.
newStartImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImportResponse
newStartImportResponse pHttpStatus_ =
  StartImportResponse'
    { resourceSpecification =
        Prelude.Nothing,
      importId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The parameters used when importing the bot or bot locale.
startImportResponse_resourceSpecification :: Lens.Lens' StartImportResponse (Prelude.Maybe ImportResourceSpecification)
startImportResponse_resourceSpecification = Lens.lens (\StartImportResponse' {resourceSpecification} -> resourceSpecification) (\s@StartImportResponse' {} a -> s {resourceSpecification = a} :: StartImportResponse)

-- | A unique identifier for the import.
startImportResponse_importId :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.Text)
startImportResponse_importId = Lens.lens (\StartImportResponse' {importId} -> importId) (\s@StartImportResponse' {} a -> s {importId = a} :: StartImportResponse)

-- | The date and time that the import request was created.
startImportResponse_creationDateTime :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.UTCTime)
startImportResponse_creationDateTime = Lens.lens (\StartImportResponse' {creationDateTime} -> creationDateTime) (\s@StartImportResponse' {} a -> s {creationDateTime = a} :: StartImportResponse) Prelude.. Lens.mapping Core._Time

-- | The strategy used when there was a name conflict between the imported
-- resource and an existing resource. When the merge strategy is
-- @FailOnConflict@ existing resources are not overwritten and the import
-- fails.
startImportResponse_mergeStrategy :: Lens.Lens' StartImportResponse (Prelude.Maybe MergeStrategy)
startImportResponse_mergeStrategy = Lens.lens (\StartImportResponse' {mergeStrategy} -> mergeStrategy) (\s@StartImportResponse' {} a -> s {mergeStrategy = a} :: StartImportResponse)

-- | The current status of the import. When the status is @Complete@ the bot
-- or bot alias is ready to use.
startImportResponse_importStatus :: Lens.Lens' StartImportResponse (Prelude.Maybe ImportStatus)
startImportResponse_importStatus = Lens.lens (\StartImportResponse' {importStatus} -> importStatus) (\s@StartImportResponse' {} a -> s {importStatus = a} :: StartImportResponse)

-- | The response's http status code.
startImportResponse_httpStatus :: Lens.Lens' StartImportResponse Prelude.Int
startImportResponse_httpStatus = Lens.lens (\StartImportResponse' {httpStatus} -> httpStatus) (\s@StartImportResponse' {} a -> s {httpStatus = a} :: StartImportResponse)

instance Prelude.NFData StartImportResponse
