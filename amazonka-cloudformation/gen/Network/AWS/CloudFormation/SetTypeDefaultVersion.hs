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
-- Module      : Network.AWS.CloudFormation.SetTypeDefaultVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the default version of an extension. The default version of an
-- extension will be used in CloudFormation operations.
module Network.AWS.CloudFormation.SetTypeDefaultVersion
  ( -- * Creating a Request
    SetTypeDefaultVersion (..),
    newSetTypeDefaultVersion,

    -- * Request Lenses
    setTypeDefaultVersion_typeName,
    setTypeDefaultVersion_arn,
    setTypeDefaultVersion_versionId,
    setTypeDefaultVersion_type,

    -- * Destructuring the Response
    SetTypeDefaultVersionResponse (..),
    newSetTypeDefaultVersionResponse,

    -- * Response Lenses
    setTypeDefaultVersionResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetTypeDefaultVersion' smart constructor.
data SetTypeDefaultVersion = SetTypeDefaultVersion'
  { -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the extension for which you want
    -- version summary information.
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
-- Create a value of 'SetTypeDefaultVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'setTypeDefaultVersion_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'arn', 'setTypeDefaultVersion_arn' - The Amazon Resource Name (ARN) of the extension for which you want
-- version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'versionId', 'setTypeDefaultVersion_versionId' - The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
--
-- 'type'', 'setTypeDefaultVersion_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
newSetTypeDefaultVersion ::
  SetTypeDefaultVersion
newSetTypeDefaultVersion =
  SetTypeDefaultVersion'
    { typeName = Core.Nothing,
      arn = Core.Nothing,
      versionId = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
setTypeDefaultVersion_typeName :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Core.Text)
setTypeDefaultVersion_typeName = Lens.lens (\SetTypeDefaultVersion' {typeName} -> typeName) (\s@SetTypeDefaultVersion' {} a -> s {typeName = a} :: SetTypeDefaultVersion)

-- | The Amazon Resource Name (ARN) of the extension for which you want
-- version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
setTypeDefaultVersion_arn :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Core.Text)
setTypeDefaultVersion_arn = Lens.lens (\SetTypeDefaultVersion' {arn} -> arn) (\s@SetTypeDefaultVersion' {} a -> s {arn = a} :: SetTypeDefaultVersion)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
setTypeDefaultVersion_versionId :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe Core.Text)
setTypeDefaultVersion_versionId = Lens.lens (\SetTypeDefaultVersion' {versionId} -> versionId) (\s@SetTypeDefaultVersion' {} a -> s {versionId = a} :: SetTypeDefaultVersion)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
setTypeDefaultVersion_type :: Lens.Lens' SetTypeDefaultVersion (Core.Maybe RegistryType)
setTypeDefaultVersion_type = Lens.lens (\SetTypeDefaultVersion' {type'} -> type') (\s@SetTypeDefaultVersion' {} a -> s {type' = a} :: SetTypeDefaultVersion)

instance Core.AWSRequest SetTypeDefaultVersion where
  type
    AWSResponse SetTypeDefaultVersion =
      SetTypeDefaultVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetTypeDefaultVersionResult"
      ( \s h x ->
          SetTypeDefaultVersionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetTypeDefaultVersion

instance Core.NFData SetTypeDefaultVersion

instance Core.ToHeaders SetTypeDefaultVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetTypeDefaultVersion where
  toPath = Core.const "/"

instance Core.ToQuery SetTypeDefaultVersion where
  toQuery SetTypeDefaultVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetTypeDefaultVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "TypeName" Core.=: typeName,
        "Arn" Core.=: arn,
        "VersionId" Core.=: versionId,
        "Type" Core.=: type'
      ]

-- | /See:/ 'newSetTypeDefaultVersionResponse' smart constructor.
data SetTypeDefaultVersionResponse = SetTypeDefaultVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetTypeDefaultVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setTypeDefaultVersionResponse_httpStatus' - The response's http status code.
newSetTypeDefaultVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetTypeDefaultVersionResponse
newSetTypeDefaultVersionResponse pHttpStatus_ =
  SetTypeDefaultVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setTypeDefaultVersionResponse_httpStatus :: Lens.Lens' SetTypeDefaultVersionResponse Core.Int
setTypeDefaultVersionResponse_httpStatus = Lens.lens (\SetTypeDefaultVersionResponse' {httpStatus} -> httpStatus) (\s@SetTypeDefaultVersionResponse' {} a -> s {httpStatus = a} :: SetTypeDefaultVersionResponse)

instance Core.NFData SetTypeDefaultVersionResponse
