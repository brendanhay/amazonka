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
-- Module      : Amazonka.CloudFormation.DeactivateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates a public extension that was previously activated in this
-- account and Region.
--
-- Once deactivated, an extension can\'t be used in any CloudFormation
-- operation. This includes stack update operations where the stack
-- template includes the extension, even if no updates are being made to
-- the extension. In addition, deactivated extensions aren\'t automatically
-- updated if a new version of the extension is released.
module Amazonka.CloudFormation.DeactivateType
  ( -- * Creating a Request
    DeactivateType (..),
    newDeactivateType,

    -- * Request Lenses
    deactivateType_arn,
    deactivateType_type,
    deactivateType_typeName,

    -- * Destructuring the Response
    DeactivateTypeResponse (..),
    newDeactivateTypeResponse,

    -- * Response Lenses
    deactivateTypeResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateType' smart constructor.
data DeactivateType = DeactivateType'
  { -- | The Amazon Resource Name (ARN) for the extension, in this account and
    -- Region.
    --
    -- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The extension type.
    --
    -- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
    type' :: Prelude.Maybe ThirdPartyType,
    -- | The type name of the extension, in this account and Region. If you
    -- specified a type name alias when enabling the extension, use the type
    -- name alias.
    --
    -- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
    typeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deactivateType_arn' - The Amazon Resource Name (ARN) for the extension, in this account and
-- Region.
--
-- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
--
-- 'type'', 'deactivateType_type' - The extension type.
--
-- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
--
-- 'typeName', 'deactivateType_typeName' - The type name of the extension, in this account and Region. If you
-- specified a type name alias when enabling the extension, use the type
-- name alias.
--
-- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
newDeactivateType ::
  DeactivateType
newDeactivateType =
  DeactivateType'
    { arn = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the extension, in this account and
-- Region.
--
-- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
deactivateType_arn :: Lens.Lens' DeactivateType (Prelude.Maybe Prelude.Text)
deactivateType_arn = Lens.lens (\DeactivateType' {arn} -> arn) (\s@DeactivateType' {} a -> s {arn = a} :: DeactivateType)

-- | The extension type.
--
-- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
deactivateType_type :: Lens.Lens' DeactivateType (Prelude.Maybe ThirdPartyType)
deactivateType_type = Lens.lens (\DeactivateType' {type'} -> type') (\s@DeactivateType' {} a -> s {type' = a} :: DeactivateType)

-- | The type name of the extension, in this account and Region. If you
-- specified a type name alias when enabling the extension, use the type
-- name alias.
--
-- Conditional: You must specify either @Arn@, or @TypeName@ and @Type@.
deactivateType_typeName :: Lens.Lens' DeactivateType (Prelude.Maybe Prelude.Text)
deactivateType_typeName = Lens.lens (\DeactivateType' {typeName} -> typeName) (\s@DeactivateType' {} a -> s {typeName = a} :: DeactivateType)

instance Core.AWSRequest DeactivateType where
  type
    AWSResponse DeactivateType =
      DeactivateTypeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeactivateTypeResult"
      ( \s h x ->
          DeactivateTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeactivateType where
  hashWithSalt _salt DeactivateType' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData DeactivateType where
  rnf DeactivateType' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf typeName

instance Data.ToHeaders DeactivateType where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeactivateType where
  toPath = Prelude.const "/"

instance Data.ToQuery DeactivateType where
  toQuery DeactivateType' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeactivateType" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Arn" Data.=: arn,
        "Type" Data.=: type',
        "TypeName" Data.=: typeName
      ]

-- | /See:/ 'newDeactivateTypeResponse' smart constructor.
data DeactivateTypeResponse = DeactivateTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivateTypeResponse_httpStatus' - The response's http status code.
newDeactivateTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeactivateTypeResponse
newDeactivateTypeResponse pHttpStatus_ =
  DeactivateTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deactivateTypeResponse_httpStatus :: Lens.Lens' DeactivateTypeResponse Prelude.Int
deactivateTypeResponse_httpStatus = Lens.lens (\DeactivateTypeResponse' {httpStatus} -> httpStatus) (\s@DeactivateTypeResponse' {} a -> s {httpStatus = a} :: DeactivateTypeResponse)

instance Prelude.NFData DeactivateTypeResponse where
  rnf DeactivateTypeResponse' {..} =
    Prelude.rnf httpStatus
