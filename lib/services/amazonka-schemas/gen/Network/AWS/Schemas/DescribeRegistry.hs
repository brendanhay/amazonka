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
-- Module      : Network.AWS.Schemas.DescribeRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the registry.
module Network.AWS.Schemas.DescribeRegistry
  ( -- * Creating a Request
    DescribeRegistry (..),
    newDescribeRegistry,

    -- * Request Lenses
    describeRegistry_registryName,

    -- * Destructuring the Response
    DescribeRegistryResponse (..),
    newDescribeRegistryResponse,

    -- * Response Lenses
    describeRegistryResponse_registryName,
    describeRegistryResponse_registryArn,
    describeRegistryResponse_description,
    describeRegistryResponse_tags,
    describeRegistryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Schemas.Types

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegistryResponse'
            Prelude.<$> (x Core..?> "RegistryName")
            Prelude.<*> (x Core..?> "RegistryArn")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRegistry

instance Prelude.NFData DescribeRegistry

instance Core.ToHeaders DescribeRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeRegistry where
  toPath DescribeRegistry' {..} =
    Prelude.mconcat
      ["/v1/registries/name/", Core.toBS registryName]

instance Core.ToQuery DescribeRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRegistryResponse' smart constructor.
data DescribeRegistryResponse = DescribeRegistryResponse'
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the registry.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'registryName', 'describeRegistryResponse_registryName' - The name of the registry.
--
-- 'registryArn', 'describeRegistryResponse_registryArn' - The ARN of the registry.
--
-- 'description', 'describeRegistryResponse_description' - The description of the registry.
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
    { registryName =
        Prelude.Nothing,
      registryArn = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the registry.
describeRegistryResponse_registryName :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_registryName = Lens.lens (\DescribeRegistryResponse' {registryName} -> registryName) (\s@DescribeRegistryResponse' {} a -> s {registryName = a} :: DescribeRegistryResponse)

-- | The ARN of the registry.
describeRegistryResponse_registryArn :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_registryArn = Lens.lens (\DescribeRegistryResponse' {registryArn} -> registryArn) (\s@DescribeRegistryResponse' {} a -> s {registryArn = a} :: DescribeRegistryResponse)

-- | The description of the registry.
describeRegistryResponse_description :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_description = Lens.lens (\DescribeRegistryResponse' {description} -> description) (\s@DescribeRegistryResponse' {} a -> s {description = a} :: DescribeRegistryResponse)

-- | Tags associated with the registry.
describeRegistryResponse_tags :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRegistryResponse_tags = Lens.lens (\DescribeRegistryResponse' {tags} -> tags) (\s@DescribeRegistryResponse' {} a -> s {tags = a} :: DescribeRegistryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRegistryResponse_httpStatus :: Lens.Lens' DescribeRegistryResponse Prelude.Int
describeRegistryResponse_httpStatus = Lens.lens (\DescribeRegistryResponse' {httpStatus} -> httpStatus) (\s@DescribeRegistryResponse' {} a -> s {httpStatus = a} :: DescribeRegistryResponse)

instance Prelude.NFData DescribeRegistryResponse
