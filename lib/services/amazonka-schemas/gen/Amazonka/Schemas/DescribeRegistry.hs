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
-- Module      : Amazonka.Schemas.DescribeRegistry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the registry.
module Amazonka.Schemas.DescribeRegistry
  ( -- * Creating a Request
    DescribeRegistry (..),
    newDescribeRegistry,

    -- * Request Lenses
    describeRegistry_registryName,

    -- * Destructuring the Response
    DescribeRegistryResponse (..),
    newDescribeRegistryResponse,

    -- * Response Lenses
    describeRegistryResponse_description,
    describeRegistryResponse_registryArn,
    describeRegistryResponse_registryName,
    describeRegistryResponse_tags,
    describeRegistryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newDescribeRegistry' smart constructor.
data DescribeRegistry = DescribeRegistry'
  { -- | The name of the registry.
    registryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'describeRegistry_registryName' - The name of the registry.
newDescribeRegistry ::
  -- | 'registryName'
  Prelude.Text ->
  DescribeRegistry
newDescribeRegistry pRegistryName_ =
  DescribeRegistry' {registryName = pRegistryName_}

-- | The name of the registry.
describeRegistry_registryName :: Lens.Lens' DescribeRegistry Prelude.Text
describeRegistry_registryName = Lens.lens (\DescribeRegistry' {registryName} -> registryName) (\s@DescribeRegistry' {} a -> s {registryName = a} :: DescribeRegistry)

instance Core.AWSRequest DescribeRegistry where
  type
    AWSResponse DescribeRegistry =
      DescribeRegistryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegistryResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "RegistryArn")
            Prelude.<*> (x Data..?> "RegistryName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRegistry where
  hashWithSalt _salt DescribeRegistry' {..} =
    _salt `Prelude.hashWithSalt` registryName

instance Prelude.NFData DescribeRegistry where
  rnf DescribeRegistry' {..} = Prelude.rnf registryName

instance Data.ToHeaders DescribeRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRegistry where
  toPath DescribeRegistry' {..} =
    Prelude.mconcat
      ["/v1/registries/name/", Data.toBS registryName]

instance Data.ToQuery DescribeRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRegistryResponse' smart constructor.
data DescribeRegistryResponse = DescribeRegistryResponse'
  { -- | The description of the registry.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the registry.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeRegistryResponse_description' - The description of the registry.
--
-- 'registryArn', 'describeRegistryResponse_registryArn' - The ARN of the registry.
--
-- 'registryName', 'describeRegistryResponse_registryName' - The name of the registry.
--
-- 'tags', 'describeRegistryResponse_tags' - Tags associated with the registry.
--
-- 'httpStatus', 'describeRegistryResponse_httpStatus' - The response's http status code.
newDescribeRegistryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRegistryResponse
newDescribeRegistryResponse pHttpStatus_ =
  DescribeRegistryResponse'
    { description =
        Prelude.Nothing,
      registryArn = Prelude.Nothing,
      registryName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the registry.
describeRegistryResponse_description :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_description = Lens.lens (\DescribeRegistryResponse' {description} -> description) (\s@DescribeRegistryResponse' {} a -> s {description = a} :: DescribeRegistryResponse)

-- | The ARN of the registry.
describeRegistryResponse_registryArn :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_registryArn = Lens.lens (\DescribeRegistryResponse' {registryArn} -> registryArn) (\s@DescribeRegistryResponse' {} a -> s {registryArn = a} :: DescribeRegistryResponse)

-- | The name of the registry.
describeRegistryResponse_registryName :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_registryName = Lens.lens (\DescribeRegistryResponse' {registryName} -> registryName) (\s@DescribeRegistryResponse' {} a -> s {registryName = a} :: DescribeRegistryResponse)

-- | Tags associated with the registry.
describeRegistryResponse_tags :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRegistryResponse_tags = Lens.lens (\DescribeRegistryResponse' {tags} -> tags) (\s@DescribeRegistryResponse' {} a -> s {tags = a} :: DescribeRegistryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRegistryResponse_httpStatus :: Lens.Lens' DescribeRegistryResponse Prelude.Int
describeRegistryResponse_httpStatus = Lens.lens (\DescribeRegistryResponse' {httpStatus} -> httpStatus) (\s@DescribeRegistryResponse' {} a -> s {httpStatus = a} :: DescribeRegistryResponse)

instance Prelude.NFData DescribeRegistryResponse where
  rnf DescribeRegistryResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf registryArn `Prelude.seq`
        Prelude.rnf registryName `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf httpStatus
