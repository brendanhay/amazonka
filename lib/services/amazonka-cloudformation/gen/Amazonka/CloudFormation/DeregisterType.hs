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
-- Module      : Amazonka.CloudFormation.DeregisterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks an extension or extension version as @DEPRECATED@ in the
-- CloudFormation registry, removing it from active use. Deprecated
-- extensions or extension versions cannot be used in CloudFormation
-- operations.
--
-- To deregister an entire extension, you must individually deregister all
-- active versions of that extension. If an extension has only a single
-- active version, deregistering that version results in the extension
-- itself being deregistered and marked as deprecated in the registry.
--
-- You can\'t deregister the default version of an extension if there are
-- other active version of that extension. If you do deregister the default
-- version of an extension, the extension type itself is deregistered as
-- well and marked as deprecated.
--
-- To view the deprecation status of an extension or extension version, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeType.html DescribeType>.
module Amazonka.CloudFormation.DeregisterType
  ( -- * Creating a Request
    DeregisterType (..),
    newDeregisterType,

    -- * Request Lenses
    deregisterType_arn,
    deregisterType_type,
    deregisterType_typeName,
    deregisterType_versionId,

    -- * Destructuring the Response
    DeregisterTypeResponse (..),
    newDeregisterTypeResponse,

    -- * Response Lenses
    deregisterTypeResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterType' smart constructor.
data DeregisterType = DeregisterType'
  { -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The kind of extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Prelude.Maybe RegistryType,
    -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a specific version of the extension. The version ID is the
    -- value at the end of the Amazon Resource Name (ARN) assigned to the
    -- extension version when it is registered.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deregisterType_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'type'', 'deregisterType_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'typeName', 'deregisterType_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'versionId', 'deregisterType_versionId' - The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
newDeregisterType ::
  DeregisterType
newDeregisterType =
  DeregisterType'
    { arn = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeName = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
deregisterType_arn :: Lens.Lens' DeregisterType (Prelude.Maybe Prelude.Text)
deregisterType_arn = Lens.lens (\DeregisterType' {arn} -> arn) (\s@DeregisterType' {} a -> s {arn = a} :: DeregisterType)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
deregisterType_type :: Lens.Lens' DeregisterType (Prelude.Maybe RegistryType)
deregisterType_type = Lens.lens (\DeregisterType' {type'} -> type') (\s@DeregisterType' {} a -> s {type' = a} :: DeregisterType)

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
deregisterType_typeName :: Lens.Lens' DeregisterType (Prelude.Maybe Prelude.Text)
deregisterType_typeName = Lens.lens (\DeregisterType' {typeName} -> typeName) (\s@DeregisterType' {} a -> s {typeName = a} :: DeregisterType)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
deregisterType_versionId :: Lens.Lens' DeregisterType (Prelude.Maybe Prelude.Text)
deregisterType_versionId = Lens.lens (\DeregisterType' {versionId} -> versionId) (\s@DeregisterType' {} a -> s {versionId = a} :: DeregisterType)

instance Core.AWSRequest DeregisterType where
  type
    AWSResponse DeregisterType =
      DeregisterTypeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeregisterTypeResult"
      ( \s h x ->
          DeregisterTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterType where
  hashWithSalt _salt DeregisterType' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData DeregisterType where
  rnf DeregisterType' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToHeaders DeregisterType where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeregisterType where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterType where
  toQuery DeregisterType' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeregisterType" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Arn" Data.=: arn,
        "Type" Data.=: type',
        "TypeName" Data.=: typeName,
        "VersionId" Data.=: versionId
      ]

-- | /See:/ 'newDeregisterTypeResponse' smart constructor.
data DeregisterTypeResponse = DeregisterTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterTypeResponse_httpStatus' - The response's http status code.
newDeregisterTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTypeResponse
newDeregisterTypeResponse pHttpStatus_ =
  DeregisterTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deregisterTypeResponse_httpStatus :: Lens.Lens' DeregisterTypeResponse Prelude.Int
deregisterTypeResponse_httpStatus = Lens.lens (\DeregisterTypeResponse' {httpStatus} -> httpStatus) (\s@DeregisterTypeResponse' {} a -> s {httpStatus = a} :: DeregisterTypeResponse)

instance Prelude.NFData DeregisterTypeResponse where
  rnf DeregisterTypeResponse' {..} =
    Prelude.rnf httpStatus
