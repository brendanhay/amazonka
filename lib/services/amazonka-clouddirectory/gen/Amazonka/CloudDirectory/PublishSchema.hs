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
-- Module      : Amazonka.CloudDirectory.PublishSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a development schema with a major version and a recommended
-- minor version.
module Amazonka.CloudDirectory.PublishSchema
  ( -- * Creating a Request
    PublishSchema (..),
    newPublishSchema,

    -- * Request Lenses
    publishSchema_minorVersion,
    publishSchema_name,
    publishSchema_developmentSchemaArn,
    publishSchema_version,

    -- * Destructuring the Response
    PublishSchemaResponse (..),
    newPublishSchemaResponse,

    -- * Response Lenses
    publishSchemaResponse_publishedSchemaArn,
    publishSchemaResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishSchema' smart constructor.
data PublishSchema = PublishSchema'
  { -- | The minor version under which the schema will be published. This
    -- parameter is recommended. Schemas have both a major and minor version
    -- associated with them.
    minorVersion :: Prelude.Maybe Prelude.Text,
    -- | The new name under which the schema will be published. If this is not
    -- provided, the development schema is considered.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the development
    -- schema. For more information, see arns.
    developmentSchemaArn :: Prelude.Text,
    -- | The major version under which the schema will be published. Schemas have
    -- both a major and minor version associated with them.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minorVersion', 'publishSchema_minorVersion' - The minor version under which the schema will be published. This
-- parameter is recommended. Schemas have both a major and minor version
-- associated with them.
--
-- 'name', 'publishSchema_name' - The new name under which the schema will be published. If this is not
-- provided, the development schema is considered.
--
-- 'developmentSchemaArn', 'publishSchema_developmentSchemaArn' - The Amazon Resource Name (ARN) that is associated with the development
-- schema. For more information, see arns.
--
-- 'version', 'publishSchema_version' - The major version under which the schema will be published. Schemas have
-- both a major and minor version associated with them.
newPublishSchema ::
  -- | 'developmentSchemaArn'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  PublishSchema
newPublishSchema pDevelopmentSchemaArn_ pVersion_ =
  PublishSchema'
    { minorVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      developmentSchemaArn = pDevelopmentSchemaArn_,
      version = pVersion_
    }

-- | The minor version under which the schema will be published. This
-- parameter is recommended. Schemas have both a major and minor version
-- associated with them.
publishSchema_minorVersion :: Lens.Lens' PublishSchema (Prelude.Maybe Prelude.Text)
publishSchema_minorVersion = Lens.lens (\PublishSchema' {minorVersion} -> minorVersion) (\s@PublishSchema' {} a -> s {minorVersion = a} :: PublishSchema)

-- | The new name under which the schema will be published. If this is not
-- provided, the development schema is considered.
publishSchema_name :: Lens.Lens' PublishSchema (Prelude.Maybe Prelude.Text)
publishSchema_name = Lens.lens (\PublishSchema' {name} -> name) (\s@PublishSchema' {} a -> s {name = a} :: PublishSchema)

-- | The Amazon Resource Name (ARN) that is associated with the development
-- schema. For more information, see arns.
publishSchema_developmentSchemaArn :: Lens.Lens' PublishSchema Prelude.Text
publishSchema_developmentSchemaArn = Lens.lens (\PublishSchema' {developmentSchemaArn} -> developmentSchemaArn) (\s@PublishSchema' {} a -> s {developmentSchemaArn = a} :: PublishSchema)

-- | The major version under which the schema will be published. Schemas have
-- both a major and minor version associated with them.
publishSchema_version :: Lens.Lens' PublishSchema Prelude.Text
publishSchema_version = Lens.lens (\PublishSchema' {version} -> version) (\s@PublishSchema' {} a -> s {version = a} :: PublishSchema)

instance Core.AWSRequest PublishSchema where
  type
    AWSResponse PublishSchema =
      PublishSchemaResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishSchemaResponse'
            Prelude.<$> (x Data..?> "PublishedSchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PublishSchema where
  hashWithSalt _salt PublishSchema' {..} =
    _salt
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` developmentSchemaArn
      `Prelude.hashWithSalt` version

instance Prelude.NFData PublishSchema where
  rnf PublishSchema' {..} =
    Prelude.rnf minorVersion `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf developmentSchemaArn `Prelude.seq`
          Prelude.rnf version

instance Data.ToHeaders PublishSchema where
  toHeaders PublishSchema' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# developmentSchemaArn]

instance Data.ToJSON PublishSchema where
  toJSON PublishSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MinorVersion" Data..=) Prelude.<$> minorVersion,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("Version" Data..= version)
          ]
      )

instance Data.ToPath PublishSchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/publish"

instance Data.ToQuery PublishSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPublishSchemaResponse' smart constructor.
data PublishSchemaResponse = PublishSchemaResponse'
  { -- | The ARN that is associated with the published schema. For more
    -- information, see arns.
    publishedSchemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publishedSchemaArn', 'publishSchemaResponse_publishedSchemaArn' - The ARN that is associated with the published schema. For more
-- information, see arns.
--
-- 'httpStatus', 'publishSchemaResponse_httpStatus' - The response's http status code.
newPublishSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishSchemaResponse
newPublishSchemaResponse pHttpStatus_ =
  PublishSchemaResponse'
    { publishedSchemaArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that is associated with the published schema. For more
-- information, see arns.
publishSchemaResponse_publishedSchemaArn :: Lens.Lens' PublishSchemaResponse (Prelude.Maybe Prelude.Text)
publishSchemaResponse_publishedSchemaArn = Lens.lens (\PublishSchemaResponse' {publishedSchemaArn} -> publishedSchemaArn) (\s@PublishSchemaResponse' {} a -> s {publishedSchemaArn = a} :: PublishSchemaResponse)

-- | The response's http status code.
publishSchemaResponse_httpStatus :: Lens.Lens' PublishSchemaResponse Prelude.Int
publishSchemaResponse_httpStatus = Lens.lens (\PublishSchemaResponse' {httpStatus} -> httpStatus) (\s@PublishSchemaResponse' {} a -> s {httpStatus = a} :: PublishSchemaResponse)

instance Prelude.NFData PublishSchemaResponse where
  rnf PublishSchemaResponse' {..} =
    Prelude.rnf publishedSchemaArn `Prelude.seq`
      Prelude.rnf httpStatus
