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
-- Module      : Amazonka.IoT.CreatePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IoT software package that can be deployed to your fleet.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreatePackage>
-- and
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetIndexingConfiguration>
-- actions.
module Amazonka.IoT.CreatePackage
  ( -- * Creating a Request
    CreatePackage (..),
    newCreatePackage,

    -- * Request Lenses
    createPackage_clientToken,
    createPackage_description,
    createPackage_tags,
    createPackage_packageName,

    -- * Destructuring the Response
    CreatePackageResponse (..),
    newCreatePackageResponse,

    -- * Response Lenses
    createPackageResponse_description,
    createPackageResponse_packageArn,
    createPackageResponse_packageName,
    createPackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePackage' smart constructor.
data CreatePackage = CreatePackage'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A summary of the package being created. This can be used to outline the
    -- package\'s contents or purpose.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Metadata that can be used to manage the package.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the new package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPackage_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'description', 'createPackage_description' - A summary of the package being created. This can be used to outline the
-- package\'s contents or purpose.
--
-- 'tags', 'createPackage_tags' - Metadata that can be used to manage the package.
--
-- 'packageName', 'createPackage_packageName' - The name of the new package.
newCreatePackage ::
  -- | 'packageName'
  Prelude.Text ->
  CreatePackage
newCreatePackage pPackageName_ =
  CreatePackage'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      packageName = pPackageName_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createPackage_clientToken :: Lens.Lens' CreatePackage (Prelude.Maybe Prelude.Text)
createPackage_clientToken = Lens.lens (\CreatePackage' {clientToken} -> clientToken) (\s@CreatePackage' {} a -> s {clientToken = a} :: CreatePackage)

-- | A summary of the package being created. This can be used to outline the
-- package\'s contents or purpose.
createPackage_description :: Lens.Lens' CreatePackage (Prelude.Maybe Prelude.Text)
createPackage_description = Lens.lens (\CreatePackage' {description} -> description) (\s@CreatePackage' {} a -> s {description = a} :: CreatePackage) Prelude.. Lens.mapping Data._Sensitive

-- | Metadata that can be used to manage the package.
createPackage_tags :: Lens.Lens' CreatePackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackage_tags = Lens.lens (\CreatePackage' {tags} -> tags) (\s@CreatePackage' {} a -> s {tags = a} :: CreatePackage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new package.
createPackage_packageName :: Lens.Lens' CreatePackage Prelude.Text
createPackage_packageName = Lens.lens (\CreatePackage' {packageName} -> packageName) (\s@CreatePackage' {} a -> s {packageName = a} :: CreatePackage)

instance Core.AWSRequest CreatePackage where
  type
    AWSResponse CreatePackage =
      CreatePackageResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackageResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "packageArn")
            Prelude.<*> (x Data..?> "packageName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePackage where
  hashWithSalt _salt CreatePackage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData CreatePackage where
  rnf CreatePackage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf packageName

instance Data.ToHeaders CreatePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreatePackage where
  toJSON CreatePackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreatePackage where
  toPath CreatePackage' {..} =
    Prelude.mconcat
      ["/packages/", Data.toBS packageName]

instance Data.ToQuery CreatePackage where
  toQuery CreatePackage' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newCreatePackageResponse' smart constructor.
data CreatePackageResponse = CreatePackageResponse'
  { -- | The package description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) for the package.
    packageArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createPackageResponse_description' - The package description.
--
-- 'packageArn', 'createPackageResponse_packageArn' - The Amazon Resource Name (ARN) for the package.
--
-- 'packageName', 'createPackageResponse_packageName' - The name of the package.
--
-- 'httpStatus', 'createPackageResponse_httpStatus' - The response's http status code.
newCreatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePackageResponse
newCreatePackageResponse pHttpStatus_ =
  CreatePackageResponse'
    { description =
        Prelude.Nothing,
      packageArn = Prelude.Nothing,
      packageName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The package description.
createPackageResponse_description :: Lens.Lens' CreatePackageResponse (Prelude.Maybe Prelude.Text)
createPackageResponse_description = Lens.lens (\CreatePackageResponse' {description} -> description) (\s@CreatePackageResponse' {} a -> s {description = a} :: CreatePackageResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) for the package.
createPackageResponse_packageArn :: Lens.Lens' CreatePackageResponse (Prelude.Maybe Prelude.Text)
createPackageResponse_packageArn = Lens.lens (\CreatePackageResponse' {packageArn} -> packageArn) (\s@CreatePackageResponse' {} a -> s {packageArn = a} :: CreatePackageResponse)

-- | The name of the package.
createPackageResponse_packageName :: Lens.Lens' CreatePackageResponse (Prelude.Maybe Prelude.Text)
createPackageResponse_packageName = Lens.lens (\CreatePackageResponse' {packageName} -> packageName) (\s@CreatePackageResponse' {} a -> s {packageName = a} :: CreatePackageResponse)

-- | The response's http status code.
createPackageResponse_httpStatus :: Lens.Lens' CreatePackageResponse Prelude.Int
createPackageResponse_httpStatus = Lens.lens (\CreatePackageResponse' {httpStatus} -> httpStatus) (\s@CreatePackageResponse' {} a -> s {httpStatus = a} :: CreatePackageResponse)

instance Prelude.NFData CreatePackageResponse where
  rnf CreatePackageResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf packageArn
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf httpStatus
