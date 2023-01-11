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
-- Module      : Amazonka.Greengrass.CreateResourceDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a resource definition that has already been
-- defined.
module Amazonka.Greengrass.CreateResourceDefinitionVersion
  ( -- * Creating a Request
    CreateResourceDefinitionVersion (..),
    newCreateResourceDefinitionVersion,

    -- * Request Lenses
    createResourceDefinitionVersion_amznClientToken,
    createResourceDefinitionVersion_resources,
    createResourceDefinitionVersion_resourceDefinitionId,

    -- * Destructuring the Response
    CreateResourceDefinitionVersionResponse (..),
    newCreateResourceDefinitionVersionResponse,

    -- * Response Lenses
    createResourceDefinitionVersionResponse_arn,
    createResourceDefinitionVersionResponse_creationTimestamp,
    createResourceDefinitionVersionResponse_id,
    createResourceDefinitionVersionResponse_version,
    createResourceDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourceDefinitionVersion' smart constructor.
data CreateResourceDefinitionVersion = CreateResourceDefinitionVersion'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of resources.
    resources :: Prelude.Maybe [Resource],
    -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createResourceDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'resources', 'createResourceDefinitionVersion_resources' - A list of resources.
--
-- 'resourceDefinitionId', 'createResourceDefinitionVersion_resourceDefinitionId' - The ID of the resource definition.
newCreateResourceDefinitionVersion ::
  -- | 'resourceDefinitionId'
  Prelude.Text ->
  CreateResourceDefinitionVersion
newCreateResourceDefinitionVersion
  pResourceDefinitionId_ =
    CreateResourceDefinitionVersion'
      { amznClientToken =
          Prelude.Nothing,
        resources = Prelude.Nothing,
        resourceDefinitionId =
          pResourceDefinitionId_
      }

-- | A client token used to correlate requests and responses.
createResourceDefinitionVersion_amznClientToken :: Lens.Lens' CreateResourceDefinitionVersion (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersion_amznClientToken = Lens.lens (\CreateResourceDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateResourceDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateResourceDefinitionVersion)

-- | A list of resources.
createResourceDefinitionVersion_resources :: Lens.Lens' CreateResourceDefinitionVersion (Prelude.Maybe [Resource])
createResourceDefinitionVersion_resources = Lens.lens (\CreateResourceDefinitionVersion' {resources} -> resources) (\s@CreateResourceDefinitionVersion' {} a -> s {resources = a} :: CreateResourceDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the resource definition.
createResourceDefinitionVersion_resourceDefinitionId :: Lens.Lens' CreateResourceDefinitionVersion Prelude.Text
createResourceDefinitionVersion_resourceDefinitionId = Lens.lens (\CreateResourceDefinitionVersion' {resourceDefinitionId} -> resourceDefinitionId) (\s@CreateResourceDefinitionVersion' {} a -> s {resourceDefinitionId = a} :: CreateResourceDefinitionVersion)

instance
  Core.AWSRequest
    CreateResourceDefinitionVersion
  where
  type
    AWSResponse CreateResourceDefinitionVersion =
      CreateResourceDefinitionVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateResourceDefinitionVersion
  where
  hashWithSalt
    _salt
    CreateResourceDefinitionVersion' {..} =
      _salt `Prelude.hashWithSalt` amznClientToken
        `Prelude.hashWithSalt` resources
        `Prelude.hashWithSalt` resourceDefinitionId

instance
  Prelude.NFData
    CreateResourceDefinitionVersion
  where
  rnf CreateResourceDefinitionVersion' {..} =
    Prelude.rnf amznClientToken
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf resourceDefinitionId

instance
  Data.ToHeaders
    CreateResourceDefinitionVersion
  where
  toHeaders CreateResourceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateResourceDefinitionVersion where
  toJSON CreateResourceDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Resources" Data..=) Prelude.<$> resources]
      )

instance Data.ToPath CreateResourceDefinitionVersion where
  toPath CreateResourceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Data.toBS resourceDefinitionId,
        "/versions"
      ]

instance Data.ToQuery CreateResourceDefinitionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceDefinitionVersionResponse' smart constructor.
data CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createResourceDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'creationTimestamp', 'createResourceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'id', 'createResourceDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createResourceDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createResourceDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateResourceDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceDefinitionVersionResponse
newCreateResourceDefinitionVersionResponse
  pHttpStatus_ =
    CreateResourceDefinitionVersionResponse'
      { arn =
          Prelude.Nothing,
        creationTimestamp =
          Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the version.
createResourceDefinitionVersionResponse_arn :: Lens.Lens' CreateResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersionResponse_arn = Lens.lens (\CreateResourceDefinitionVersionResponse' {arn} -> arn) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {arn = a} :: CreateResourceDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the version was created.
createResourceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateResourceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateResourceDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createResourceDefinitionVersionResponse_id :: Lens.Lens' CreateResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersionResponse_id = Lens.lens (\CreateResourceDefinitionVersionResponse' {id} -> id) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {id = a} :: CreateResourceDefinitionVersionResponse)

-- | The ID of the version.
createResourceDefinitionVersionResponse_version :: Lens.Lens' CreateResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersionResponse_version = Lens.lens (\CreateResourceDefinitionVersionResponse' {version} -> version) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {version = a} :: CreateResourceDefinitionVersionResponse)

-- | The response's http status code.
createResourceDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateResourceDefinitionVersionResponse Prelude.Int
createResourceDefinitionVersionResponse_httpStatus = Lens.lens (\CreateResourceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateResourceDefinitionVersionResponse)

instance
  Prelude.NFData
    CreateResourceDefinitionVersionResponse
  where
  rnf CreateResourceDefinitionVersionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
