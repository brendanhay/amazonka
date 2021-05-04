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
-- Module      : Network.AWS.Greengrass.CreateResourceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a resource definition that has already been
-- defined.
module Network.AWS.Greengrass.CreateResourceDefinitionVersion
  ( -- * Creating a Request
    CreateResourceDefinitionVersion (..),
    newCreateResourceDefinitionVersion,

    -- * Request Lenses
    createResourceDefinitionVersion_resources,
    createResourceDefinitionVersion_amznClientToken,
    createResourceDefinitionVersion_resourceDefinitionId,

    -- * Destructuring the Response
    CreateResourceDefinitionVersionResponse (..),
    newCreateResourceDefinitionVersionResponse,

    -- * Response Lenses
    createResourceDefinitionVersionResponse_creationTimestamp,
    createResourceDefinitionVersionResponse_arn,
    createResourceDefinitionVersionResponse_id,
    createResourceDefinitionVersionResponse_version,
    createResourceDefinitionVersionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateResourceDefinitionVersion' smart constructor.
data CreateResourceDefinitionVersion = CreateResourceDefinitionVersion'
  { -- | A list of resources.
    resources :: Prelude.Maybe [Resource],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'createResourceDefinitionVersion_resources' - A list of resources.
--
-- 'amznClientToken', 'createResourceDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'resourceDefinitionId', 'createResourceDefinitionVersion_resourceDefinitionId' - The ID of the resource definition.
newCreateResourceDefinitionVersion ::
  -- | 'resourceDefinitionId'
  Prelude.Text ->
  CreateResourceDefinitionVersion
newCreateResourceDefinitionVersion
  pResourceDefinitionId_ =
    CreateResourceDefinitionVersion'
      { resources =
          Prelude.Nothing,
        amznClientToken = Prelude.Nothing,
        resourceDefinitionId =
          pResourceDefinitionId_
      }

-- | A list of resources.
createResourceDefinitionVersion_resources :: Lens.Lens' CreateResourceDefinitionVersion (Prelude.Maybe [Resource])
createResourceDefinitionVersion_resources = Lens.lens (\CreateResourceDefinitionVersion' {resources} -> resources) (\s@CreateResourceDefinitionVersion' {} a -> s {resources = a} :: CreateResourceDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

-- | A client token used to correlate requests and responses.
createResourceDefinitionVersion_amznClientToken :: Lens.Lens' CreateResourceDefinitionVersion (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersion_amznClientToken = Lens.lens (\CreateResourceDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateResourceDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateResourceDefinitionVersion)

-- | The ID of the resource definition.
createResourceDefinitionVersion_resourceDefinitionId :: Lens.Lens' CreateResourceDefinitionVersion Prelude.Text
createResourceDefinitionVersion_resourceDefinitionId = Lens.lens (\CreateResourceDefinitionVersion' {resourceDefinitionId} -> resourceDefinitionId) (\s@CreateResourceDefinitionVersion' {} a -> s {resourceDefinitionId = a} :: CreateResourceDefinitionVersion)

instance
  Prelude.AWSRequest
    CreateResourceDefinitionVersion
  where
  type
    Rs CreateResourceDefinitionVersion =
      CreateResourceDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionVersionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateResourceDefinitionVersion

instance
  Prelude.NFData
    CreateResourceDefinitionVersion

instance
  Prelude.ToHeaders
    CreateResourceDefinitionVersion
  where
  toHeaders CreateResourceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance
  Prelude.ToJSON
    CreateResourceDefinitionVersion
  where
  toJSON CreateResourceDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Resources" Prelude..=) Prelude.<$> resources]
      )

instance
  Prelude.ToPath
    CreateResourceDefinitionVersion
  where
  toPath CreateResourceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Prelude.toBS resourceDefinitionId,
        "/versions"
      ]

instance
  Prelude.ToQuery
    CreateResourceDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceDefinitionVersionResponse' smart constructor.
data CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createResourceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createResourceDefinitionVersionResponse_arn' - The ARN of the version.
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
      { creationTimestamp =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the version was created.
createResourceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateResourceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateResourceDefinitionVersionResponse)

-- | The ARN of the version.
createResourceDefinitionVersionResponse_arn :: Lens.Lens' CreateResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createResourceDefinitionVersionResponse_arn = Lens.lens (\CreateResourceDefinitionVersionResponse' {arn} -> arn) (\s@CreateResourceDefinitionVersionResponse' {} a -> s {arn = a} :: CreateResourceDefinitionVersionResponse)

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
