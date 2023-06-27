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
-- Module      : Amazonka.RAM.ReplacePermissionAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates all resource shares that use a managed permission to a different
-- managed permission. This operation always applies the default version of
-- the target managed permission. You can optionally specify that the
-- update applies to only resource shares that currently use a specified
-- version. This enables you to update to the latest version, without
-- changing the which managed permission is used.
--
-- You can use this operation to update all of your resource shares to use
-- the current default version of the permission by specifying the same
-- value for the @fromPermissionArn@ and @toPermissionArn@ parameters.
--
-- You can use the optional @fromPermissionVersion@ parameter to update
-- only those resources that use a specified version of the managed
-- permission to the new managed permission.
--
-- To successfully perform this operation, you must have permission to
-- update the resource-based policy on all affected resource types.
module Amazonka.RAM.ReplacePermissionAssociations
  ( -- * Creating a Request
    ReplacePermissionAssociations (..),
    newReplacePermissionAssociations,

    -- * Request Lenses
    replacePermissionAssociations_clientToken,
    replacePermissionAssociations_fromPermissionVersion,
    replacePermissionAssociations_fromPermissionArn,
    replacePermissionAssociations_toPermissionArn,

    -- * Destructuring the Response
    ReplacePermissionAssociationsResponse (..),
    newReplacePermissionAssociationsResponse,

    -- * Response Lenses
    replacePermissionAssociationsResponse_clientToken,
    replacePermissionAssociationsResponse_replacePermissionAssociationsWork,
    replacePermissionAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplacePermissionAssociations' smart constructor.
data ReplacePermissionAssociations = ReplacePermissionAssociations'
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
    -- | Specifies that you want to updated the permissions for only those
    -- resource shares that use the specified version of the managed
    -- permission.
    fromPermissionVersion :: Prelude.Maybe Prelude.Int,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the managed permission that you want to replace.
    fromPermissionArn :: Prelude.Text,
    -- | Specifies the ARN of the managed permission that you want to associate
    -- with resource shares in place of the one specified by @fromPerssionArn@
    -- and @fromPermissionVersion@.
    --
    -- The operation always associates the version that is currently the
    -- default for the specified managed permission.
    toPermissionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplacePermissionAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'replacePermissionAssociations_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'fromPermissionVersion', 'replacePermissionAssociations_fromPermissionVersion' - Specifies that you want to updated the permissions for only those
-- resource shares that use the specified version of the managed
-- permission.
--
-- 'fromPermissionArn', 'replacePermissionAssociations_fromPermissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the managed permission that you want to replace.
--
-- 'toPermissionArn', 'replacePermissionAssociations_toPermissionArn' - Specifies the ARN of the managed permission that you want to associate
-- with resource shares in place of the one specified by @fromPerssionArn@
-- and @fromPermissionVersion@.
--
-- The operation always associates the version that is currently the
-- default for the specified managed permission.
newReplacePermissionAssociations ::
  -- | 'fromPermissionArn'
  Prelude.Text ->
  -- | 'toPermissionArn'
  Prelude.Text ->
  ReplacePermissionAssociations
newReplacePermissionAssociations
  pFromPermissionArn_
  pToPermissionArn_ =
    ReplacePermissionAssociations'
      { clientToken =
          Prelude.Nothing,
        fromPermissionVersion = Prelude.Nothing,
        fromPermissionArn = pFromPermissionArn_,
        toPermissionArn = pToPermissionArn_
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
replacePermissionAssociations_clientToken :: Lens.Lens' ReplacePermissionAssociations (Prelude.Maybe Prelude.Text)
replacePermissionAssociations_clientToken = Lens.lens (\ReplacePermissionAssociations' {clientToken} -> clientToken) (\s@ReplacePermissionAssociations' {} a -> s {clientToken = a} :: ReplacePermissionAssociations)

-- | Specifies that you want to updated the permissions for only those
-- resource shares that use the specified version of the managed
-- permission.
replacePermissionAssociations_fromPermissionVersion :: Lens.Lens' ReplacePermissionAssociations (Prelude.Maybe Prelude.Int)
replacePermissionAssociations_fromPermissionVersion = Lens.lens (\ReplacePermissionAssociations' {fromPermissionVersion} -> fromPermissionVersion) (\s@ReplacePermissionAssociations' {} a -> s {fromPermissionVersion = a} :: ReplacePermissionAssociations)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the managed permission that you want to replace.
replacePermissionAssociations_fromPermissionArn :: Lens.Lens' ReplacePermissionAssociations Prelude.Text
replacePermissionAssociations_fromPermissionArn = Lens.lens (\ReplacePermissionAssociations' {fromPermissionArn} -> fromPermissionArn) (\s@ReplacePermissionAssociations' {} a -> s {fromPermissionArn = a} :: ReplacePermissionAssociations)

-- | Specifies the ARN of the managed permission that you want to associate
-- with resource shares in place of the one specified by @fromPerssionArn@
-- and @fromPermissionVersion@.
--
-- The operation always associates the version that is currently the
-- default for the specified managed permission.
replacePermissionAssociations_toPermissionArn :: Lens.Lens' ReplacePermissionAssociations Prelude.Text
replacePermissionAssociations_toPermissionArn = Lens.lens (\ReplacePermissionAssociations' {toPermissionArn} -> toPermissionArn) (\s@ReplacePermissionAssociations' {} a -> s {toPermissionArn = a} :: ReplacePermissionAssociations)

instance
  Core.AWSRequest
    ReplacePermissionAssociations
  where
  type
    AWSResponse ReplacePermissionAssociations =
      ReplacePermissionAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReplacePermissionAssociationsResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "replacePermissionAssociationsWork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ReplacePermissionAssociations
  where
  hashWithSalt _salt ReplacePermissionAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` fromPermissionVersion
      `Prelude.hashWithSalt` fromPermissionArn
      `Prelude.hashWithSalt` toPermissionArn

instance Prelude.NFData ReplacePermissionAssociations where
  rnf ReplacePermissionAssociations' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf fromPermissionVersion
      `Prelude.seq` Prelude.rnf fromPermissionArn
      `Prelude.seq` Prelude.rnf toPermissionArn

instance Data.ToHeaders ReplacePermissionAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReplacePermissionAssociations where
  toJSON ReplacePermissionAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("fromPermissionVersion" Data..=)
              Prelude.<$> fromPermissionVersion,
            Prelude.Just
              ("fromPermissionArn" Data..= fromPermissionArn),
            Prelude.Just
              ("toPermissionArn" Data..= toPermissionArn)
          ]
      )

instance Data.ToPath ReplacePermissionAssociations where
  toPath =
    Prelude.const "/replacepermissionassociations"

instance Data.ToQuery ReplacePermissionAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReplacePermissionAssociationsResponse' smart constructor.
data ReplacePermissionAssociationsResponse = ReplacePermissionAssociationsResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a data structure that you can use to track the asynchronous
    -- tasks that RAM performs to complete this operation. You can use the
    -- ListReplacePermissionAssociationsWork operation and pass the @id@ value
    -- returned in this structure.
    replacePermissionAssociationsWork :: Prelude.Maybe ReplacePermissionAssociationsWork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplacePermissionAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'replacePermissionAssociationsResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'replacePermissionAssociationsWork', 'replacePermissionAssociationsResponse_replacePermissionAssociationsWork' - Specifies a data structure that you can use to track the asynchronous
-- tasks that RAM performs to complete this operation. You can use the
-- ListReplacePermissionAssociationsWork operation and pass the @id@ value
-- returned in this structure.
--
-- 'httpStatus', 'replacePermissionAssociationsResponse_httpStatus' - The response's http status code.
newReplacePermissionAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplacePermissionAssociationsResponse
newReplacePermissionAssociationsResponse pHttpStatus_ =
  ReplacePermissionAssociationsResponse'
    { clientToken =
        Prelude.Nothing,
      replacePermissionAssociationsWork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
replacePermissionAssociationsResponse_clientToken :: Lens.Lens' ReplacePermissionAssociationsResponse (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsResponse_clientToken = Lens.lens (\ReplacePermissionAssociationsResponse' {clientToken} -> clientToken) (\s@ReplacePermissionAssociationsResponse' {} a -> s {clientToken = a} :: ReplacePermissionAssociationsResponse)

-- | Specifies a data structure that you can use to track the asynchronous
-- tasks that RAM performs to complete this operation. You can use the
-- ListReplacePermissionAssociationsWork operation and pass the @id@ value
-- returned in this structure.
replacePermissionAssociationsResponse_replacePermissionAssociationsWork :: Lens.Lens' ReplacePermissionAssociationsResponse (Prelude.Maybe ReplacePermissionAssociationsWork)
replacePermissionAssociationsResponse_replacePermissionAssociationsWork = Lens.lens (\ReplacePermissionAssociationsResponse' {replacePermissionAssociationsWork} -> replacePermissionAssociationsWork) (\s@ReplacePermissionAssociationsResponse' {} a -> s {replacePermissionAssociationsWork = a} :: ReplacePermissionAssociationsResponse)

-- | The response's http status code.
replacePermissionAssociationsResponse_httpStatus :: Lens.Lens' ReplacePermissionAssociationsResponse Prelude.Int
replacePermissionAssociationsResponse_httpStatus = Lens.lens (\ReplacePermissionAssociationsResponse' {httpStatus} -> httpStatus) (\s@ReplacePermissionAssociationsResponse' {} a -> s {httpStatus = a} :: ReplacePermissionAssociationsResponse)

instance
  Prelude.NFData
    ReplacePermissionAssociationsResponse
  where
  rnf ReplacePermissionAssociationsResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf replacePermissionAssociationsWork
      `Prelude.seq` Prelude.rnf httpStatus
