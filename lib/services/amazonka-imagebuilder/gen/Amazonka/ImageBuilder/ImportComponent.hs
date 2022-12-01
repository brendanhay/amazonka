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
-- Module      : Amazonka.ImageBuilder.ImportComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a component and transforms its data into a component document.
module Amazonka.ImageBuilder.ImportComponent
  ( -- * Creating a Request
    ImportComponent (..),
    newImportComponent,

    -- * Request Lenses
    importComponent_tags,
    importComponent_changeDescription,
    importComponent_description,
    importComponent_uri,
    importComponent_kmsKeyId,
    importComponent_data,
    importComponent_name,
    importComponent_semanticVersion,
    importComponent_type,
    importComponent_format,
    importComponent_platform,
    importComponent_clientToken,

    -- * Destructuring the Response
    ImportComponentResponse (..),
    newImportComponentResponse,

    -- * Response Lenses
    importComponentResponse_clientToken,
    importComponentResponse_requestId,
    importComponentResponse_componentBuildVersionArn,
    importComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportComponent' smart constructor.
data ImportComponent = ImportComponent'
  { -- | The tags of the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The change description of the component. Describes what change has been
    -- made in this version, or what makes this version different from other
    -- versions of this component.
    changeDescription :: Prelude.Maybe Prelude.Text,
    -- | The description of the component. Describes the contents of the
    -- component.
    description :: Prelude.Maybe Prelude.Text,
    -- | The uri of the component. Must be an Amazon S3 URL and the requester
    -- must have permission to access the Amazon S3 bucket. If you use Amazon
    -- S3, you can specify component content up to your service quota. Either
    -- @data@ or @uri@ can be used to specify the data within the component.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key that should be used to encrypt this component.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The data of the component. Used to specify the data inline. Either
    -- @data@ or @uri@ can be used to specify the data within the component.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Text,
    -- | The semantic version of the component. This version follows the semantic
    -- version syntax.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Filtering:__ With semantic versioning, you have the flexibility to use
    -- wildcards (x) to specify the most recent versions or nodes when
    -- selecting the base image or components for your recipe. When you use a
    -- wildcard in any node, all nodes to the right of the first wildcard must
    -- also be wildcards.
    semanticVersion :: Prelude.Text,
    -- | The type of the component denotes whether the component is used to build
    -- the image, or only to test it.
    type' :: ComponentType,
    -- | The format of the resource that you want to import as a component.
    format :: ComponentFormat,
    -- | The platform of the component.
    platform :: Platform,
    -- | The idempotency token of the component.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'importComponent_tags' - The tags of the component.
--
-- 'changeDescription', 'importComponent_changeDescription' - The change description of the component. Describes what change has been
-- made in this version, or what makes this version different from other
-- versions of this component.
--
-- 'description', 'importComponent_description' - The description of the component. Describes the contents of the
-- component.
--
-- 'uri', 'importComponent_uri' - The uri of the component. Must be an Amazon S3 URL and the requester
-- must have permission to access the Amazon S3 bucket. If you use Amazon
-- S3, you can specify component content up to your service quota. Either
-- @data@ or @uri@ can be used to specify the data within the component.
--
-- 'kmsKeyId', 'importComponent_kmsKeyId' - The ID of the KMS key that should be used to encrypt this component.
--
-- 'data'', 'importComponent_data' - The data of the component. Used to specify the data inline. Either
-- @data@ or @uri@ can be used to specify the data within the component.
--
-- 'name', 'importComponent_name' - The name of the component.
--
-- 'semanticVersion', 'importComponent_semanticVersion' - The semantic version of the component. This version follows the semantic
-- version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
--
-- 'type'', 'importComponent_type' - The type of the component denotes whether the component is used to build
-- the image, or only to test it.
--
-- 'format', 'importComponent_format' - The format of the resource that you want to import as a component.
--
-- 'platform', 'importComponent_platform' - The platform of the component.
--
-- 'clientToken', 'importComponent_clientToken' - The idempotency token of the component.
newImportComponent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  -- | 'type''
  ComponentType ->
  -- | 'format'
  ComponentFormat ->
  -- | 'platform'
  Platform ->
  -- | 'clientToken'
  Prelude.Text ->
  ImportComponent
newImportComponent
  pName_
  pSemanticVersion_
  pType_
  pFormat_
  pPlatform_
  pClientToken_ =
    ImportComponent'
      { tags = Prelude.Nothing,
        changeDescription = Prelude.Nothing,
        description = Prelude.Nothing,
        uri = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        data' = Prelude.Nothing,
        name = pName_,
        semanticVersion = pSemanticVersion_,
        type' = pType_,
        format = pFormat_,
        platform = pPlatform_,
        clientToken = pClientToken_
      }

-- | The tags of the component.
importComponent_tags :: Lens.Lens' ImportComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importComponent_tags = Lens.lens (\ImportComponent' {tags} -> tags) (\s@ImportComponent' {} a -> s {tags = a} :: ImportComponent) Prelude.. Lens.mapping Lens.coerced

-- | The change description of the component. Describes what change has been
-- made in this version, or what makes this version different from other
-- versions of this component.
importComponent_changeDescription :: Lens.Lens' ImportComponent (Prelude.Maybe Prelude.Text)
importComponent_changeDescription = Lens.lens (\ImportComponent' {changeDescription} -> changeDescription) (\s@ImportComponent' {} a -> s {changeDescription = a} :: ImportComponent)

-- | The description of the component. Describes the contents of the
-- component.
importComponent_description :: Lens.Lens' ImportComponent (Prelude.Maybe Prelude.Text)
importComponent_description = Lens.lens (\ImportComponent' {description} -> description) (\s@ImportComponent' {} a -> s {description = a} :: ImportComponent)

-- | The uri of the component. Must be an Amazon S3 URL and the requester
-- must have permission to access the Amazon S3 bucket. If you use Amazon
-- S3, you can specify component content up to your service quota. Either
-- @data@ or @uri@ can be used to specify the data within the component.
importComponent_uri :: Lens.Lens' ImportComponent (Prelude.Maybe Prelude.Text)
importComponent_uri = Lens.lens (\ImportComponent' {uri} -> uri) (\s@ImportComponent' {} a -> s {uri = a} :: ImportComponent)

-- | The ID of the KMS key that should be used to encrypt this component.
importComponent_kmsKeyId :: Lens.Lens' ImportComponent (Prelude.Maybe Prelude.Text)
importComponent_kmsKeyId = Lens.lens (\ImportComponent' {kmsKeyId} -> kmsKeyId) (\s@ImportComponent' {} a -> s {kmsKeyId = a} :: ImportComponent)

-- | The data of the component. Used to specify the data inline. Either
-- @data@ or @uri@ can be used to specify the data within the component.
importComponent_data :: Lens.Lens' ImportComponent (Prelude.Maybe Prelude.Text)
importComponent_data = Lens.lens (\ImportComponent' {data'} -> data') (\s@ImportComponent' {} a -> s {data' = a} :: ImportComponent)

-- | The name of the component.
importComponent_name :: Lens.Lens' ImportComponent Prelude.Text
importComponent_name = Lens.lens (\ImportComponent' {name} -> name) (\s@ImportComponent' {} a -> s {name = a} :: ImportComponent)

-- | The semantic version of the component. This version follows the semantic
-- version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
importComponent_semanticVersion :: Lens.Lens' ImportComponent Prelude.Text
importComponent_semanticVersion = Lens.lens (\ImportComponent' {semanticVersion} -> semanticVersion) (\s@ImportComponent' {} a -> s {semanticVersion = a} :: ImportComponent)

-- | The type of the component denotes whether the component is used to build
-- the image, or only to test it.
importComponent_type :: Lens.Lens' ImportComponent ComponentType
importComponent_type = Lens.lens (\ImportComponent' {type'} -> type') (\s@ImportComponent' {} a -> s {type' = a} :: ImportComponent)

-- | The format of the resource that you want to import as a component.
importComponent_format :: Lens.Lens' ImportComponent ComponentFormat
importComponent_format = Lens.lens (\ImportComponent' {format} -> format) (\s@ImportComponent' {} a -> s {format = a} :: ImportComponent)

-- | The platform of the component.
importComponent_platform :: Lens.Lens' ImportComponent Platform
importComponent_platform = Lens.lens (\ImportComponent' {platform} -> platform) (\s@ImportComponent' {} a -> s {platform = a} :: ImportComponent)

-- | The idempotency token of the component.
importComponent_clientToken :: Lens.Lens' ImportComponent Prelude.Text
importComponent_clientToken = Lens.lens (\ImportComponent' {clientToken} -> clientToken) (\s@ImportComponent' {} a -> s {clientToken = a} :: ImportComponent)

instance Core.AWSRequest ImportComponent where
  type
    AWSResponse ImportComponent =
      ImportComponentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportComponentResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "componentBuildVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportComponent where
  hashWithSalt _salt ImportComponent' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` changeDescription
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData ImportComponent where
  rnf ImportComponent' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf changeDescription
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders ImportComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportComponent where
  toJSON ImportComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("changeDescription" Core..=)
              Prelude.<$> changeDescription,
            ("description" Core..=) Prelude.<$> description,
            ("uri" Core..=) Prelude.<$> uri,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("data" Core..=) Prelude.<$> data',
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("semanticVersion" Core..= semanticVersion),
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("format" Core..= format),
            Prelude.Just ("platform" Core..= platform),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath ImportComponent where
  toPath = Prelude.const "/ImportComponent"

instance Core.ToQuery ImportComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportComponentResponse' smart constructor.
data ImportComponentResponse = ImportComponentResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the imported component.
    componentBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'importComponentResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'requestId', 'importComponentResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'componentBuildVersionArn', 'importComponentResponse_componentBuildVersionArn' - The Amazon Resource Name (ARN) of the imported component.
--
-- 'httpStatus', 'importComponentResponse_httpStatus' - The response's http status code.
newImportComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportComponentResponse
newImportComponentResponse pHttpStatus_ =
  ImportComponentResponse'
    { clientToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      componentBuildVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token used to make this request idempotent.
importComponentResponse_clientToken :: Lens.Lens' ImportComponentResponse (Prelude.Maybe Prelude.Text)
importComponentResponse_clientToken = Lens.lens (\ImportComponentResponse' {clientToken} -> clientToken) (\s@ImportComponentResponse' {} a -> s {clientToken = a} :: ImportComponentResponse)

-- | The request ID that uniquely identifies this request.
importComponentResponse_requestId :: Lens.Lens' ImportComponentResponse (Prelude.Maybe Prelude.Text)
importComponentResponse_requestId = Lens.lens (\ImportComponentResponse' {requestId} -> requestId) (\s@ImportComponentResponse' {} a -> s {requestId = a} :: ImportComponentResponse)

-- | The Amazon Resource Name (ARN) of the imported component.
importComponentResponse_componentBuildVersionArn :: Lens.Lens' ImportComponentResponse (Prelude.Maybe Prelude.Text)
importComponentResponse_componentBuildVersionArn = Lens.lens (\ImportComponentResponse' {componentBuildVersionArn} -> componentBuildVersionArn) (\s@ImportComponentResponse' {} a -> s {componentBuildVersionArn = a} :: ImportComponentResponse)

-- | The response's http status code.
importComponentResponse_httpStatus :: Lens.Lens' ImportComponentResponse Prelude.Int
importComponentResponse_httpStatus = Lens.lens (\ImportComponentResponse' {httpStatus} -> httpStatus) (\s@ImportComponentResponse' {} a -> s {httpStatus = a} :: ImportComponentResponse)

instance Prelude.NFData ImportComponentResponse where
  rnf ImportComponentResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf componentBuildVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
