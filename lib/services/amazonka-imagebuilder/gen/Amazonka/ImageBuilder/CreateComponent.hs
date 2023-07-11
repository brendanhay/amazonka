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
-- Module      : Amazonka.ImageBuilder.CreateComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new component that can be used to build, validate, test, and
-- assess your image. The component is based on a YAML document that you
-- specify using exactly one of the following methods:
--
-- -   Inline, using the @data@ property in the request body.
--
-- -   A URL that points to a YAML document file stored in Amazon S3, using
--     the @uri@ property in the request body.
module Amazonka.ImageBuilder.CreateComponent
  ( -- * Creating a Request
    CreateComponent (..),
    newCreateComponent,

    -- * Request Lenses
    createComponent_changeDescription,
    createComponent_data,
    createComponent_description,
    createComponent_kmsKeyId,
    createComponent_supportedOsVersions,
    createComponent_tags,
    createComponent_uri,
    createComponent_name,
    createComponent_semanticVersion,
    createComponent_platform,
    createComponent_clientToken,

    -- * Destructuring the Response
    CreateComponentResponse (..),
    newCreateComponentResponse,

    -- * Response Lenses
    createComponentResponse_clientToken,
    createComponentResponse_componentBuildVersionArn,
    createComponentResponse_requestId,
    createComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateComponent' smart constructor.
data CreateComponent = CreateComponent'
  { -- | The change description of the component. Describes what change has been
    -- made in this version, or what makes this version different from other
    -- versions of this component.
    changeDescription :: Prelude.Maybe Prelude.Text,
    -- | Component @data@ contains inline YAML document content for the
    -- component. Alternatively, you can specify the @uri@ of a YAML document
    -- file stored in Amazon S3. However, you cannot specify both properties.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The description of the component. Describes the contents of the
    -- component.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key that should be used to encrypt this component.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The operating system (OS) version supported by the component. If the OS
    -- information is available, a prefix match is performed against the base
    -- image OS version during image recipe creation.
    supportedOsVersions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The tags of the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The @uri@ of a YAML component document file. This must be an S3 URL
    -- (@s3:\/\/bucket\/key@), and the requester must have permission to access
    -- the S3 bucket it points to. If you use Amazon S3, you can specify
    -- component content up to your service quota.
    --
    -- Alternatively, you can specify the YAML document inline, using the
    -- component @data@ property. You cannot specify both properties.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Text,
    -- | The semantic version of the component. This version follows the semantic
    -- version syntax.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Assignment:__ For the first three nodes you can assign any positive
    -- integer value, including zero, with an upper limit of 2^30-1, or
    -- 1073741823 for each node. Image Builder automatically assigns the build
    -- number to the fourth node.
    --
    -- __Patterns:__ You can use any numeric pattern that adheres to the
    -- assignment requirements for the nodes that you can assign. For example,
    -- you might choose a software version pattern, such as 1.0.0, or a date,
    -- such as 2021.01.01.
    semanticVersion :: Prelude.Text,
    -- | The platform of the component.
    platform :: Platform,
    -- | The idempotency token of the component.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeDescription', 'createComponent_changeDescription' - The change description of the component. Describes what change has been
-- made in this version, or what makes this version different from other
-- versions of this component.
--
-- 'data'', 'createComponent_data' - Component @data@ contains inline YAML document content for the
-- component. Alternatively, you can specify the @uri@ of a YAML document
-- file stored in Amazon S3. However, you cannot specify both properties.
--
-- 'description', 'createComponent_description' - The description of the component. Describes the contents of the
-- component.
--
-- 'kmsKeyId', 'createComponent_kmsKeyId' - The ID of the KMS key that should be used to encrypt this component.
--
-- 'supportedOsVersions', 'createComponent_supportedOsVersions' - The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
--
-- 'tags', 'createComponent_tags' - The tags of the component.
--
-- 'uri', 'createComponent_uri' - The @uri@ of a YAML component document file. This must be an S3 URL
-- (@s3:\/\/bucket\/key@), and the requester must have permission to access
-- the S3 bucket it points to. If you use Amazon S3, you can specify
-- component content up to your service quota.
--
-- Alternatively, you can specify the YAML document inline, using the
-- component @data@ property. You cannot specify both properties.
--
-- 'name', 'createComponent_name' - The name of the component.
--
-- 'semanticVersion', 'createComponent_semanticVersion' - The semantic version of the component. This version follows the semantic
-- version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- 'platform', 'createComponent_platform' - The platform of the component.
--
-- 'clientToken', 'createComponent_clientToken' - The idempotency token of the component.
newCreateComponent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  -- | 'platform'
  Platform ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateComponent
newCreateComponent
  pName_
  pSemanticVersion_
  pPlatform_
  pClientToken_ =
    CreateComponent'
      { changeDescription =
          Prelude.Nothing,
        data' = Prelude.Nothing,
        description = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        supportedOsVersions = Prelude.Nothing,
        tags = Prelude.Nothing,
        uri = Prelude.Nothing,
        name = pName_,
        semanticVersion = pSemanticVersion_,
        platform = pPlatform_,
        clientToken = pClientToken_
      }

-- | The change description of the component. Describes what change has been
-- made in this version, or what makes this version different from other
-- versions of this component.
createComponent_changeDescription :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_changeDescription = Lens.lens (\CreateComponent' {changeDescription} -> changeDescription) (\s@CreateComponent' {} a -> s {changeDescription = a} :: CreateComponent)

-- | Component @data@ contains inline YAML document content for the
-- component. Alternatively, you can specify the @uri@ of a YAML document
-- file stored in Amazon S3. However, you cannot specify both properties.
createComponent_data :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_data = Lens.lens (\CreateComponent' {data'} -> data') (\s@CreateComponent' {} a -> s {data' = a} :: CreateComponent)

-- | The description of the component. Describes the contents of the
-- component.
createComponent_description :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_description = Lens.lens (\CreateComponent' {description} -> description) (\s@CreateComponent' {} a -> s {description = a} :: CreateComponent)

-- | The ID of the KMS key that should be used to encrypt this component.
createComponent_kmsKeyId :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_kmsKeyId = Lens.lens (\CreateComponent' {kmsKeyId} -> kmsKeyId) (\s@CreateComponent' {} a -> s {kmsKeyId = a} :: CreateComponent)

-- | The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
createComponent_supportedOsVersions :: Lens.Lens' CreateComponent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createComponent_supportedOsVersions = Lens.lens (\CreateComponent' {supportedOsVersions} -> supportedOsVersions) (\s@CreateComponent' {} a -> s {supportedOsVersions = a} :: CreateComponent) Prelude.. Lens.mapping Lens.coerced

-- | The tags of the component.
createComponent_tags :: Lens.Lens' CreateComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createComponent_tags = Lens.lens (\CreateComponent' {tags} -> tags) (\s@CreateComponent' {} a -> s {tags = a} :: CreateComponent) Prelude.. Lens.mapping Lens.coerced

-- | The @uri@ of a YAML component document file. This must be an S3 URL
-- (@s3:\/\/bucket\/key@), and the requester must have permission to access
-- the S3 bucket it points to. If you use Amazon S3, you can specify
-- component content up to your service quota.
--
-- Alternatively, you can specify the YAML document inline, using the
-- component @data@ property. You cannot specify both properties.
createComponent_uri :: Lens.Lens' CreateComponent (Prelude.Maybe Prelude.Text)
createComponent_uri = Lens.lens (\CreateComponent' {uri} -> uri) (\s@CreateComponent' {} a -> s {uri = a} :: CreateComponent)

-- | The name of the component.
createComponent_name :: Lens.Lens' CreateComponent Prelude.Text
createComponent_name = Lens.lens (\CreateComponent' {name} -> name) (\s@CreateComponent' {} a -> s {name = a} :: CreateComponent)

-- | The semantic version of the component. This version follows the semantic
-- version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
createComponent_semanticVersion :: Lens.Lens' CreateComponent Prelude.Text
createComponent_semanticVersion = Lens.lens (\CreateComponent' {semanticVersion} -> semanticVersion) (\s@CreateComponent' {} a -> s {semanticVersion = a} :: CreateComponent)

-- | The platform of the component.
createComponent_platform :: Lens.Lens' CreateComponent Platform
createComponent_platform = Lens.lens (\CreateComponent' {platform} -> platform) (\s@CreateComponent' {} a -> s {platform = a} :: CreateComponent)

-- | The idempotency token of the component.
createComponent_clientToken :: Lens.Lens' CreateComponent Prelude.Text
createComponent_clientToken = Lens.lens (\CreateComponent' {clientToken} -> clientToken) (\s@CreateComponent' {} a -> s {clientToken = a} :: CreateComponent)

instance Core.AWSRequest CreateComponent where
  type
    AWSResponse CreateComponent =
      CreateComponentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComponentResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "componentBuildVersionArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComponent where
  hashWithSalt _salt CreateComponent' {..} =
    _salt
      `Prelude.hashWithSalt` changeDescription
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` supportedOsVersions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateComponent where
  rnf CreateComponent' {..} =
    Prelude.rnf changeDescription
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf supportedOsVersions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateComponent where
  toJSON CreateComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("changeDescription" Data..=)
              Prelude.<$> changeDescription,
            ("data" Data..=) Prelude.<$> data',
            ("description" Data..=) Prelude.<$> description,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("supportedOsVersions" Data..=)
              Prelude.<$> supportedOsVersions,
            ("tags" Data..=) Prelude.<$> tags,
            ("uri" Data..=) Prelude.<$> uri,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("semanticVersion" Data..= semanticVersion),
            Prelude.Just ("platform" Data..= platform),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateComponent where
  toPath = Prelude.const "/CreateComponent"

instance Data.ToQuery CreateComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateComponentResponse' smart constructor.
data CreateComponentResponse = CreateComponentResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the component that was created by this
    -- request.
    componentBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createComponentResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'componentBuildVersionArn', 'createComponentResponse_componentBuildVersionArn' - The Amazon Resource Name (ARN) of the component that was created by this
-- request.
--
-- 'requestId', 'createComponentResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'createComponentResponse_httpStatus' - The response's http status code.
newCreateComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateComponentResponse
newCreateComponentResponse pHttpStatus_ =
  CreateComponentResponse'
    { clientToken =
        Prelude.Nothing,
      componentBuildVersionArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token used to make this request idempotent.
createComponentResponse_clientToken :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Prelude.Text)
createComponentResponse_clientToken = Lens.lens (\CreateComponentResponse' {clientToken} -> clientToken) (\s@CreateComponentResponse' {} a -> s {clientToken = a} :: CreateComponentResponse)

-- | The Amazon Resource Name (ARN) of the component that was created by this
-- request.
createComponentResponse_componentBuildVersionArn :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Prelude.Text)
createComponentResponse_componentBuildVersionArn = Lens.lens (\CreateComponentResponse' {componentBuildVersionArn} -> componentBuildVersionArn) (\s@CreateComponentResponse' {} a -> s {componentBuildVersionArn = a} :: CreateComponentResponse)

-- | The request ID that uniquely identifies this request.
createComponentResponse_requestId :: Lens.Lens' CreateComponentResponse (Prelude.Maybe Prelude.Text)
createComponentResponse_requestId = Lens.lens (\CreateComponentResponse' {requestId} -> requestId) (\s@CreateComponentResponse' {} a -> s {requestId = a} :: CreateComponentResponse)

-- | The response's http status code.
createComponentResponse_httpStatus :: Lens.Lens' CreateComponentResponse Prelude.Int
createComponentResponse_httpStatus = Lens.lens (\CreateComponentResponse' {httpStatus} -> httpStatus) (\s@CreateComponentResponse' {} a -> s {httpStatus = a} :: CreateComponentResponse)

instance Prelude.NFData CreateComponentResponse where
  rnf CreateComponentResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf componentBuildVersionArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
