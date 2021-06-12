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
-- Module      : Network.AWS.CloudFormation.DeregisterType
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- You cannot deregister the default version of an extension if there are
-- other active version of that extension. If you do deregister the default
-- version of an extension, the textensionype itself is deregistered as
-- well and marked as deprecated.
--
-- To view the deprecation status of an extension or extension version, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeType.html DescribeType>.
module Network.AWS.CloudFormation.DeregisterType
  ( -- * Creating a Request
    DeregisterType (..),
    newDeregisterType,

    -- * Request Lenses
    deregisterType_typeName,
    deregisterType_arn,
    deregisterType_versionId,
    deregisterType_type,

    -- * Destructuring the Response
    DeregisterTypeResponse (..),
    newDeregisterTypeResponse,

    -- * Response Lenses
    deregisterTypeResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterType' smart constructor.
data DeregisterType = DeregisterType'
  { -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    arn :: Core.Maybe Core.Text,
    -- | The ID of a specific version of the extension. The version ID is the
    -- value at the end of the Amazon Resource Name (ARN) assigned to the
    -- extension version when it is registered.
    versionId :: Core.Maybe Core.Text,
    -- | The kind of extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Core.Maybe RegistryType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'deregisterType_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'arn', 'deregisterType_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'versionId', 'deregisterType_versionId' - The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
--
-- 'type'', 'deregisterType_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
newDeregisterType ::
  DeregisterType
newDeregisterType =
  DeregisterType'
    { typeName = Core.Nothing,
      arn = Core.Nothing,
      versionId = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
deregisterType_typeName :: Lens.Lens' DeregisterType (Core.Maybe Core.Text)
deregisterType_typeName = Lens.lens (\DeregisterType' {typeName} -> typeName) (\s@DeregisterType' {} a -> s {typeName = a} :: DeregisterType)

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
deregisterType_arn :: Lens.Lens' DeregisterType (Core.Maybe Core.Text)
deregisterType_arn = Lens.lens (\DeregisterType' {arn} -> arn) (\s@DeregisterType' {} a -> s {arn = a} :: DeregisterType)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
deregisterType_versionId :: Lens.Lens' DeregisterType (Core.Maybe Core.Text)
deregisterType_versionId = Lens.lens (\DeregisterType' {versionId} -> versionId) (\s@DeregisterType' {} a -> s {versionId = a} :: DeregisterType)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
deregisterType_type :: Lens.Lens' DeregisterType (Core.Maybe RegistryType)
deregisterType_type = Lens.lens (\DeregisterType' {type'} -> type') (\s@DeregisterType' {} a -> s {type' = a} :: DeregisterType)

instance Core.AWSRequest DeregisterType where
  type
    AWSResponse DeregisterType =
      DeregisterTypeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeregisterTypeResult"
      ( \s h x ->
          DeregisterTypeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterType

instance Core.NFData DeregisterType

instance Core.ToHeaders DeregisterType where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeregisterType where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterType where
  toQuery DeregisterType' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeregisterType" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "TypeName" Core.=: typeName,
        "Arn" Core.=: arn,
        "VersionId" Core.=: versionId,
        "Type" Core.=: type'
      ]

-- | /See:/ 'newDeregisterTypeResponse' smart constructor.
data DeregisterTypeResponse = DeregisterTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeregisterTypeResponse
newDeregisterTypeResponse pHttpStatus_ =
  DeregisterTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deregisterTypeResponse_httpStatus :: Lens.Lens' DeregisterTypeResponse Core.Int
deregisterTypeResponse_httpStatus = Lens.lens (\DeregisterTypeResponse' {httpStatus} -> httpStatus) (\s@DeregisterTypeResponse' {} a -> s {httpStatus = a} :: DeregisterTypeResponse)

instance Core.NFData DeregisterTypeResponse
