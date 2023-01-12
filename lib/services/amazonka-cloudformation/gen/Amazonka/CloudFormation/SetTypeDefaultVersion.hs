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
-- Module      : Amazonka.CloudFormation.SetTypeDefaultVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the default version of an extension. The default version of an
-- extension will be used in CloudFormation operations.
module Amazonka.CloudFormation.SetTypeDefaultVersion
  ( -- * Creating a Request
    SetTypeDefaultVersion (..),
    newSetTypeDefaultVersion,

    -- * Request Lenses
    setTypeDefaultVersion_arn,
    setTypeDefaultVersion_type,
    setTypeDefaultVersion_typeName,
    setTypeDefaultVersion_versionId,

    -- * Destructuring the Response
    SetTypeDefaultVersionResponse (..),
    newSetTypeDefaultVersionResponse,

    -- * Response Lenses
    setTypeDefaultVersionResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetTypeDefaultVersion' smart constructor.
data SetTypeDefaultVersion = SetTypeDefaultVersion'
  { -- | The Amazon Resource Name (ARN) of the extension for which you want
    -- version summary information.
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
-- Create a value of 'SetTypeDefaultVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'setTypeDefaultVersion_arn' - The Amazon Resource Name (ARN) of the extension for which you want
-- version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'type'', 'setTypeDefaultVersion_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'typeName', 'setTypeDefaultVersion_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'versionId', 'setTypeDefaultVersion_versionId' - The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
newSetTypeDefaultVersion ::
  SetTypeDefaultVersion
newSetTypeDefaultVersion =
  SetTypeDefaultVersion'
    { arn = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeName = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the extension for which you want
-- version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
setTypeDefaultVersion_arn :: Lens.Lens' SetTypeDefaultVersion (Prelude.Maybe Prelude.Text)
setTypeDefaultVersion_arn = Lens.lens (\SetTypeDefaultVersion' {arn} -> arn) (\s@SetTypeDefaultVersion' {} a -> s {arn = a} :: SetTypeDefaultVersion)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
setTypeDefaultVersion_type :: Lens.Lens' SetTypeDefaultVersion (Prelude.Maybe RegistryType)
setTypeDefaultVersion_type = Lens.lens (\SetTypeDefaultVersion' {type'} -> type') (\s@SetTypeDefaultVersion' {} a -> s {type' = a} :: SetTypeDefaultVersion)

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
setTypeDefaultVersion_typeName :: Lens.Lens' SetTypeDefaultVersion (Prelude.Maybe Prelude.Text)
setTypeDefaultVersion_typeName = Lens.lens (\SetTypeDefaultVersion' {typeName} -> typeName) (\s@SetTypeDefaultVersion' {} a -> s {typeName = a} :: SetTypeDefaultVersion)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
setTypeDefaultVersion_versionId :: Lens.Lens' SetTypeDefaultVersion (Prelude.Maybe Prelude.Text)
setTypeDefaultVersion_versionId = Lens.lens (\SetTypeDefaultVersion' {versionId} -> versionId) (\s@SetTypeDefaultVersion' {} a -> s {versionId = a} :: SetTypeDefaultVersion)

instance Core.AWSRequest SetTypeDefaultVersion where
  type
    AWSResponse SetTypeDefaultVersion =
      SetTypeDefaultVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetTypeDefaultVersionResult"
      ( \s h x ->
          SetTypeDefaultVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetTypeDefaultVersion where
  hashWithSalt _salt SetTypeDefaultVersion' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData SetTypeDefaultVersion where
  rnf SetTypeDefaultVersion' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToHeaders SetTypeDefaultVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetTypeDefaultVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery SetTypeDefaultVersion where
  toQuery SetTypeDefaultVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetTypeDefaultVersion" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Arn" Data.=: arn,
        "Type" Data.=: type',
        "TypeName" Data.=: typeName,
        "VersionId" Data.=: versionId
      ]

-- | /See:/ 'newSetTypeDefaultVersionResponse' smart constructor.
data SetTypeDefaultVersionResponse = SetTypeDefaultVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SetTypeDefaultVersionResponse
newSetTypeDefaultVersionResponse pHttpStatus_ =
  SetTypeDefaultVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setTypeDefaultVersionResponse_httpStatus :: Lens.Lens' SetTypeDefaultVersionResponse Prelude.Int
setTypeDefaultVersionResponse_httpStatus = Lens.lens (\SetTypeDefaultVersionResponse' {httpStatus} -> httpStatus) (\s@SetTypeDefaultVersionResponse' {} a -> s {httpStatus = a} :: SetTypeDefaultVersionResponse)

instance Prelude.NFData SetTypeDefaultVersionResponse where
  rnf SetTypeDefaultVersionResponse' {..} =
    Prelude.rnf httpStatus
