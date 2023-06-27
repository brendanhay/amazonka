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
-- Module      : Amazonka.RAM.SetDefaultPermissionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the specified version number as the default version for the
-- specified customer managed permission. New resource shares automatically
-- use this new default permission. Existing resource shares continue to
-- use their original permission version, but you can use
-- ReplacePermissionAssociations to update them.
module Amazonka.RAM.SetDefaultPermissionVersion
  ( -- * Creating a Request
    SetDefaultPermissionVersion (..),
    newSetDefaultPermissionVersion,

    -- * Request Lenses
    setDefaultPermissionVersion_clientToken,
    setDefaultPermissionVersion_permissionArn,
    setDefaultPermissionVersion_permissionVersion,

    -- * Destructuring the Response
    SetDefaultPermissionVersionResponse (..),
    newSetDefaultPermissionVersionResponse,

    -- * Response Lenses
    setDefaultPermissionVersionResponse_clientToken,
    setDefaultPermissionVersionResponse_returnValue,
    setDefaultPermissionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetDefaultPermissionVersion' smart constructor.
data SetDefaultPermissionVersion = SetDefaultPermissionVersion'
  { -- | Specifies a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    --
    -- If you retry the operation with the same @ClientToken@, but with
    -- different parameters, the retry fails with an
    -- @IdempotentParameterMismatch@ error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the customer managed permission whose default version you want to
    -- change.
    permissionArn :: Prelude.Text,
    -- | Specifies the version number that you want to designate as the default
    -- for customer managed permission. To see a list of all available version
    -- numbers, use ListPermissionVersions.
    permissionVersion :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultPermissionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'setDefaultPermissionVersion_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
--
-- 'permissionArn', 'setDefaultPermissionVersion_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the customer managed permission whose default version you want to
-- change.
--
-- 'permissionVersion', 'setDefaultPermissionVersion_permissionVersion' - Specifies the version number that you want to designate as the default
-- for customer managed permission. To see a list of all available version
-- numbers, use ListPermissionVersions.
newSetDefaultPermissionVersion ::
  -- | 'permissionArn'
  Prelude.Text ->
  -- | 'permissionVersion'
  Prelude.Int ->
  SetDefaultPermissionVersion
newSetDefaultPermissionVersion
  pPermissionArn_
  pPermissionVersion_ =
    SetDefaultPermissionVersion'
      { clientToken =
          Prelude.Nothing,
        permissionArn = pPermissionArn_,
        permissionVersion = pPermissionVersion_
      }

-- | Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
setDefaultPermissionVersion_clientToken :: Lens.Lens' SetDefaultPermissionVersion (Prelude.Maybe Prelude.Text)
setDefaultPermissionVersion_clientToken = Lens.lens (\SetDefaultPermissionVersion' {clientToken} -> clientToken) (\s@SetDefaultPermissionVersion' {} a -> s {clientToken = a} :: SetDefaultPermissionVersion)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the customer managed permission whose default version you want to
-- change.
setDefaultPermissionVersion_permissionArn :: Lens.Lens' SetDefaultPermissionVersion Prelude.Text
setDefaultPermissionVersion_permissionArn = Lens.lens (\SetDefaultPermissionVersion' {permissionArn} -> permissionArn) (\s@SetDefaultPermissionVersion' {} a -> s {permissionArn = a} :: SetDefaultPermissionVersion)

-- | Specifies the version number that you want to designate as the default
-- for customer managed permission. To see a list of all available version
-- numbers, use ListPermissionVersions.
setDefaultPermissionVersion_permissionVersion :: Lens.Lens' SetDefaultPermissionVersion Prelude.Int
setDefaultPermissionVersion_permissionVersion = Lens.lens (\SetDefaultPermissionVersion' {permissionVersion} -> permissionVersion) (\s@SetDefaultPermissionVersion' {} a -> s {permissionVersion = a} :: SetDefaultPermissionVersion)

instance Core.AWSRequest SetDefaultPermissionVersion where
  type
    AWSResponse SetDefaultPermissionVersion =
      SetDefaultPermissionVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetDefaultPermissionVersionResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetDefaultPermissionVersion where
  hashWithSalt _salt SetDefaultPermissionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` permissionArn
      `Prelude.hashWithSalt` permissionVersion

instance Prelude.NFData SetDefaultPermissionVersion where
  rnf SetDefaultPermissionVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf permissionVersion

instance Data.ToHeaders SetDefaultPermissionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetDefaultPermissionVersion where
  toJSON SetDefaultPermissionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("permissionArn" Data..= permissionArn),
            Prelude.Just
              ("permissionVersion" Data..= permissionVersion)
          ]
      )

instance Data.ToPath SetDefaultPermissionVersion where
  toPath = Prelude.const "/setdefaultpermissionversion"

instance Data.ToQuery SetDefaultPermissionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetDefaultPermissionVersionResponse' smart constructor.
data SetDefaultPermissionVersionResponse = SetDefaultPermissionVersionResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A boolean value that indicates whether the operation was successful.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultPermissionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'setDefaultPermissionVersionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'returnValue', 'setDefaultPermissionVersionResponse_returnValue' - A boolean value that indicates whether the operation was successful.
--
-- 'httpStatus', 'setDefaultPermissionVersionResponse_httpStatus' - The response's http status code.
newSetDefaultPermissionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetDefaultPermissionVersionResponse
newSetDefaultPermissionVersionResponse pHttpStatus_ =
  SetDefaultPermissionVersionResponse'
    { clientToken =
        Prelude.Nothing,
      returnValue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
setDefaultPermissionVersionResponse_clientToken :: Lens.Lens' SetDefaultPermissionVersionResponse (Prelude.Maybe Prelude.Text)
setDefaultPermissionVersionResponse_clientToken = Lens.lens (\SetDefaultPermissionVersionResponse' {clientToken} -> clientToken) (\s@SetDefaultPermissionVersionResponse' {} a -> s {clientToken = a} :: SetDefaultPermissionVersionResponse)

-- | A boolean value that indicates whether the operation was successful.
setDefaultPermissionVersionResponse_returnValue :: Lens.Lens' SetDefaultPermissionVersionResponse (Prelude.Maybe Prelude.Bool)
setDefaultPermissionVersionResponse_returnValue = Lens.lens (\SetDefaultPermissionVersionResponse' {returnValue} -> returnValue) (\s@SetDefaultPermissionVersionResponse' {} a -> s {returnValue = a} :: SetDefaultPermissionVersionResponse)

-- | The response's http status code.
setDefaultPermissionVersionResponse_httpStatus :: Lens.Lens' SetDefaultPermissionVersionResponse Prelude.Int
setDefaultPermissionVersionResponse_httpStatus = Lens.lens (\SetDefaultPermissionVersionResponse' {httpStatus} -> httpStatus) (\s@SetDefaultPermissionVersionResponse' {} a -> s {httpStatus = a} :: SetDefaultPermissionVersionResponse)

instance
  Prelude.NFData
    SetDefaultPermissionVersionResponse
  where
  rnf SetDefaultPermissionVersionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
