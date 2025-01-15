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
-- Module      : Amazonka.AppStream.CreateAppBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an app block.
--
-- App blocks are an Amazon AppStream 2.0 resource that stores the details
-- about the virtual hard disk in an S3 bucket. It also stores the setup
-- script with details about how to mount the virtual hard disk. The
-- virtual hard disk includes the application binaries and other files
-- necessary to launch your applications. Multiple applications can be
-- assigned to a single app block.
--
-- This is only supported for Elastic fleets.
module Amazonka.AppStream.CreateAppBlock
  ( -- * Creating a Request
    CreateAppBlock (..),
    newCreateAppBlock,

    -- * Request Lenses
    createAppBlock_description,
    createAppBlock_displayName,
    createAppBlock_tags,
    createAppBlock_name,
    createAppBlock_sourceS3Location,
    createAppBlock_setupScriptDetails,

    -- * Destructuring the Response
    CreateAppBlockResponse (..),
    newCreateAppBlockResponse,

    -- * Response Lenses
    createAppBlockResponse_appBlock,
    createAppBlockResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAppBlock' smart constructor.
data CreateAppBlock = CreateAppBlock'
  { -- | The description of the app block.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the app block. This is not displayed to the user.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the app block.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the app block.
    name :: Prelude.Text,
    -- | The source S3 location of the app block.
    sourceS3Location :: S3Location,
    -- | The setup script details of the app block.
    setupScriptDetails :: ScriptDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAppBlock_description' - The description of the app block.
--
-- 'displayName', 'createAppBlock_displayName' - The display name of the app block. This is not displayed to the user.
--
-- 'tags', 'createAppBlock_tags' - The tags assigned to the app block.
--
-- 'name', 'createAppBlock_name' - The name of the app block.
--
-- 'sourceS3Location', 'createAppBlock_sourceS3Location' - The source S3 location of the app block.
--
-- 'setupScriptDetails', 'createAppBlock_setupScriptDetails' - The setup script details of the app block.
newCreateAppBlock ::
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceS3Location'
  S3Location ->
  -- | 'setupScriptDetails'
  ScriptDetails ->
  CreateAppBlock
newCreateAppBlock
  pName_
  pSourceS3Location_
  pSetupScriptDetails_ =
    CreateAppBlock'
      { description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        sourceS3Location = pSourceS3Location_,
        setupScriptDetails = pSetupScriptDetails_
      }

-- | The description of the app block.
createAppBlock_description :: Lens.Lens' CreateAppBlock (Prelude.Maybe Prelude.Text)
createAppBlock_description = Lens.lens (\CreateAppBlock' {description} -> description) (\s@CreateAppBlock' {} a -> s {description = a} :: CreateAppBlock)

-- | The display name of the app block. This is not displayed to the user.
createAppBlock_displayName :: Lens.Lens' CreateAppBlock (Prelude.Maybe Prelude.Text)
createAppBlock_displayName = Lens.lens (\CreateAppBlock' {displayName} -> displayName) (\s@CreateAppBlock' {} a -> s {displayName = a} :: CreateAppBlock)

-- | The tags assigned to the app block.
createAppBlock_tags :: Lens.Lens' CreateAppBlock (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAppBlock_tags = Lens.lens (\CreateAppBlock' {tags} -> tags) (\s@CreateAppBlock' {} a -> s {tags = a} :: CreateAppBlock) Prelude.. Lens.mapping Lens.coerced

-- | The name of the app block.
createAppBlock_name :: Lens.Lens' CreateAppBlock Prelude.Text
createAppBlock_name = Lens.lens (\CreateAppBlock' {name} -> name) (\s@CreateAppBlock' {} a -> s {name = a} :: CreateAppBlock)

-- | The source S3 location of the app block.
createAppBlock_sourceS3Location :: Lens.Lens' CreateAppBlock S3Location
createAppBlock_sourceS3Location = Lens.lens (\CreateAppBlock' {sourceS3Location} -> sourceS3Location) (\s@CreateAppBlock' {} a -> s {sourceS3Location = a} :: CreateAppBlock)

-- | The setup script details of the app block.
createAppBlock_setupScriptDetails :: Lens.Lens' CreateAppBlock ScriptDetails
createAppBlock_setupScriptDetails = Lens.lens (\CreateAppBlock' {setupScriptDetails} -> setupScriptDetails) (\s@CreateAppBlock' {} a -> s {setupScriptDetails = a} :: CreateAppBlock)

instance Core.AWSRequest CreateAppBlock where
  type
    AWSResponse CreateAppBlock =
      CreateAppBlockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppBlockResponse'
            Prelude.<$> (x Data..?> "AppBlock")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppBlock where
  hashWithSalt _salt CreateAppBlock' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceS3Location
      `Prelude.hashWithSalt` setupScriptDetails

instance Prelude.NFData CreateAppBlock where
  rnf CreateAppBlock' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf displayName `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf sourceS3Location `Prelude.seq`
              Prelude.rnf setupScriptDetails

instance Data.ToHeaders CreateAppBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateAppBlock" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAppBlock where
  toJSON CreateAppBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("SourceS3Location" Data..= sourceS3Location),
            Prelude.Just
              ("SetupScriptDetails" Data..= setupScriptDetails)
          ]
      )

instance Data.ToPath CreateAppBlock where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAppBlock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppBlockResponse' smart constructor.
data CreateAppBlockResponse = CreateAppBlockResponse'
  { -- | The app block.
    appBlock :: Prelude.Maybe AppBlock,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBlock', 'createAppBlockResponse_appBlock' - The app block.
--
-- 'httpStatus', 'createAppBlockResponse_httpStatus' - The response's http status code.
newCreateAppBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppBlockResponse
newCreateAppBlockResponse pHttpStatus_ =
  CreateAppBlockResponse'
    { appBlock = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app block.
createAppBlockResponse_appBlock :: Lens.Lens' CreateAppBlockResponse (Prelude.Maybe AppBlock)
createAppBlockResponse_appBlock = Lens.lens (\CreateAppBlockResponse' {appBlock} -> appBlock) (\s@CreateAppBlockResponse' {} a -> s {appBlock = a} :: CreateAppBlockResponse)

-- | The response's http status code.
createAppBlockResponse_httpStatus :: Lens.Lens' CreateAppBlockResponse Prelude.Int
createAppBlockResponse_httpStatus = Lens.lens (\CreateAppBlockResponse' {httpStatus} -> httpStatus) (\s@CreateAppBlockResponse' {} a -> s {httpStatus = a} :: CreateAppBlockResponse)

instance Prelude.NFData CreateAppBlockResponse where
  rnf CreateAppBlockResponse' {..} =
    Prelude.rnf appBlock `Prelude.seq`
      Prelude.rnf httpStatus
