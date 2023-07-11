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
-- Module      : Amazonka.Proton.CreateEnvironmentTemplateVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new major or minor version of an environment template. A major
-- version of an environment template is a version that /isn\'t/ backwards
-- compatible. A minor version of an environment template is a version
-- that\'s backwards compatible within its major version.
module Amazonka.Proton.CreateEnvironmentTemplateVersion
  ( -- * Creating a Request
    CreateEnvironmentTemplateVersion (..),
    newCreateEnvironmentTemplateVersion,

    -- * Request Lenses
    createEnvironmentTemplateVersion_clientToken,
    createEnvironmentTemplateVersion_description,
    createEnvironmentTemplateVersion_majorVersion,
    createEnvironmentTemplateVersion_tags,
    createEnvironmentTemplateVersion_source,
    createEnvironmentTemplateVersion_templateName,

    -- * Destructuring the Response
    CreateEnvironmentTemplateVersionResponse (..),
    newCreateEnvironmentTemplateVersionResponse,

    -- * Response Lenses
    createEnvironmentTemplateVersionResponse_httpStatus,
    createEnvironmentTemplateVersionResponse_environmentTemplateVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironmentTemplateVersion' smart constructor.
data CreateEnvironmentTemplateVersion = CreateEnvironmentTemplateVersion'
  { -- | When included, if two identical requests are made with the same client
    -- token, Proton returns the environment template version that the first
    -- request created.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the new version of an environment template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | To create a new minor version of the environment template, include
    -- @major Version@.
    --
    -- To create a new major and minor version of the environment template,
    -- exclude @major Version@.
    majorVersion :: Prelude.Maybe Prelude.Text,
    -- | An optional list of metadata items that you can associate with the
    -- Proton environment template version. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | An object that includes the template bundle S3 bucket path and name for
    -- the new version of an template.
    source :: TemplateVersionSourceInput,
    -- | The name of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createEnvironmentTemplateVersion_clientToken' - When included, if two identical requests are made with the same client
-- token, Proton returns the environment template version that the first
-- request created.
--
-- 'description', 'createEnvironmentTemplateVersion_description' - A description of the new version of an environment template.
--
-- 'majorVersion', 'createEnvironmentTemplateVersion_majorVersion' - To create a new minor version of the environment template, include
-- @major Version@.
--
-- To create a new major and minor version of the environment template,
-- exclude @major Version@.
--
-- 'tags', 'createEnvironmentTemplateVersion_tags' - An optional list of metadata items that you can associate with the
-- Proton environment template version. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'source', 'createEnvironmentTemplateVersion_source' - An object that includes the template bundle S3 bucket path and name for
-- the new version of an template.
--
-- 'templateName', 'createEnvironmentTemplateVersion_templateName' - The name of the environment template.
newCreateEnvironmentTemplateVersion ::
  -- | 'source'
  TemplateVersionSourceInput ->
  -- | 'templateName'
  Prelude.Text ->
  CreateEnvironmentTemplateVersion
newCreateEnvironmentTemplateVersion
  pSource_
  pTemplateName_ =
    CreateEnvironmentTemplateVersion'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        majorVersion = Prelude.Nothing,
        tags = Prelude.Nothing,
        source = pSource_,
        templateName = pTemplateName_
      }

-- | When included, if two identical requests are made with the same client
-- token, Proton returns the environment template version that the first
-- request created.
createEnvironmentTemplateVersion_clientToken :: Lens.Lens' CreateEnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
createEnvironmentTemplateVersion_clientToken = Lens.lens (\CreateEnvironmentTemplateVersion' {clientToken} -> clientToken) (\s@CreateEnvironmentTemplateVersion' {} a -> s {clientToken = a} :: CreateEnvironmentTemplateVersion)

-- | A description of the new version of an environment template.
createEnvironmentTemplateVersion_description :: Lens.Lens' CreateEnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
createEnvironmentTemplateVersion_description = Lens.lens (\CreateEnvironmentTemplateVersion' {description} -> description) (\s@CreateEnvironmentTemplateVersion' {} a -> s {description = a} :: CreateEnvironmentTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | To create a new minor version of the environment template, include
-- @major Version@.
--
-- To create a new major and minor version of the environment template,
-- exclude @major Version@.
createEnvironmentTemplateVersion_majorVersion :: Lens.Lens' CreateEnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
createEnvironmentTemplateVersion_majorVersion = Lens.lens (\CreateEnvironmentTemplateVersion' {majorVersion} -> majorVersion) (\s@CreateEnvironmentTemplateVersion' {} a -> s {majorVersion = a} :: CreateEnvironmentTemplateVersion)

-- | An optional list of metadata items that you can associate with the
-- Proton environment template version. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createEnvironmentTemplateVersion_tags :: Lens.Lens' CreateEnvironmentTemplateVersion (Prelude.Maybe [Tag])
createEnvironmentTemplateVersion_tags = Lens.lens (\CreateEnvironmentTemplateVersion' {tags} -> tags) (\s@CreateEnvironmentTemplateVersion' {} a -> s {tags = a} :: CreateEnvironmentTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | An object that includes the template bundle S3 bucket path and name for
-- the new version of an template.
createEnvironmentTemplateVersion_source :: Lens.Lens' CreateEnvironmentTemplateVersion TemplateVersionSourceInput
createEnvironmentTemplateVersion_source = Lens.lens (\CreateEnvironmentTemplateVersion' {source} -> source) (\s@CreateEnvironmentTemplateVersion' {} a -> s {source = a} :: CreateEnvironmentTemplateVersion)

-- | The name of the environment template.
createEnvironmentTemplateVersion_templateName :: Lens.Lens' CreateEnvironmentTemplateVersion Prelude.Text
createEnvironmentTemplateVersion_templateName = Lens.lens (\CreateEnvironmentTemplateVersion' {templateName} -> templateName) (\s@CreateEnvironmentTemplateVersion' {} a -> s {templateName = a} :: CreateEnvironmentTemplateVersion)

instance
  Core.AWSRequest
    CreateEnvironmentTemplateVersion
  where
  type
    AWSResponse CreateEnvironmentTemplateVersion =
      CreateEnvironmentTemplateVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentTemplateVersion")
      )

instance
  Prelude.Hashable
    CreateEnvironmentTemplateVersion
  where
  hashWithSalt
    _salt
    CreateEnvironmentTemplateVersion' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` majorVersion
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    CreateEnvironmentTemplateVersion
  where
  rnf CreateEnvironmentTemplateVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf templateName

instance
  Data.ToHeaders
    CreateEnvironmentTemplateVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateEnvironmentTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEnvironmentTemplateVersion where
  toJSON CreateEnvironmentTemplateVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("majorVersion" Data..=) Prelude.<$> majorVersion,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("source" Data..= source),
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )

instance Data.ToPath CreateEnvironmentTemplateVersion where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateEnvironmentTemplateVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentTemplateVersionResponse' smart constructor.
data CreateEnvironmentTemplateVersionResponse = CreateEnvironmentTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment template detail data that\'s returned by Proton.
    environmentTemplateVersion :: EnvironmentTemplateVersion
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEnvironmentTemplateVersionResponse_httpStatus' - The response's http status code.
--
-- 'environmentTemplateVersion', 'createEnvironmentTemplateVersionResponse_environmentTemplateVersion' - The environment template detail data that\'s returned by Proton.
newCreateEnvironmentTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentTemplateVersion'
  EnvironmentTemplateVersion ->
  CreateEnvironmentTemplateVersionResponse
newCreateEnvironmentTemplateVersionResponse
  pHttpStatus_
  pEnvironmentTemplateVersion_ =
    CreateEnvironmentTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentTemplateVersion =
          pEnvironmentTemplateVersion_
      }

-- | The response's http status code.
createEnvironmentTemplateVersionResponse_httpStatus :: Lens.Lens' CreateEnvironmentTemplateVersionResponse Prelude.Int
createEnvironmentTemplateVersionResponse_httpStatus = Lens.lens (\CreateEnvironmentTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentTemplateVersionResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentTemplateVersionResponse)

-- | The environment template detail data that\'s returned by Proton.
createEnvironmentTemplateVersionResponse_environmentTemplateVersion :: Lens.Lens' CreateEnvironmentTemplateVersionResponse EnvironmentTemplateVersion
createEnvironmentTemplateVersionResponse_environmentTemplateVersion = Lens.lens (\CreateEnvironmentTemplateVersionResponse' {environmentTemplateVersion} -> environmentTemplateVersion) (\s@CreateEnvironmentTemplateVersionResponse' {} a -> s {environmentTemplateVersion = a} :: CreateEnvironmentTemplateVersionResponse)

instance
  Prelude.NFData
    CreateEnvironmentTemplateVersionResponse
  where
  rnf CreateEnvironmentTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentTemplateVersion
