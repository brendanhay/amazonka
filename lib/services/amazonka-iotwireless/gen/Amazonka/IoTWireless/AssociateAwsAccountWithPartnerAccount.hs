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
-- Module      : Amazonka.IoTWireless.AssociateAwsAccountWithPartnerAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a partner account with your AWS account.
module Amazonka.IoTWireless.AssociateAwsAccountWithPartnerAccount
  ( -- * Creating a Request
    AssociateAwsAccountWithPartnerAccount (..),
    newAssociateAwsAccountWithPartnerAccount,

    -- * Request Lenses
    associateAwsAccountWithPartnerAccount_clientRequestToken,
    associateAwsAccountWithPartnerAccount_tags,
    associateAwsAccountWithPartnerAccount_sidewalk,

    -- * Destructuring the Response
    AssociateAwsAccountWithPartnerAccountResponse (..),
    newAssociateAwsAccountWithPartnerAccountResponse,

    -- * Response Lenses
    associateAwsAccountWithPartnerAccountResponse_arn,
    associateAwsAccountWithPartnerAccountResponse_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateAwsAccountWithPartnerAccount' smart constructor.
data AssociateAwsAccountWithPartnerAccount = AssociateAwsAccountWithPartnerAccount'
  { -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The tags to attach to the specified resource. Tags are metadata that you
    -- can use to manage a resource.
    tags :: Prelude.Maybe [Tag],
    -- | The Sidewalk account credentials.
    sidewalk :: SidewalkAccountInfo
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAwsAccountWithPartnerAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateAwsAccountWithPartnerAccount_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'tags', 'associateAwsAccountWithPartnerAccount_tags' - The tags to attach to the specified resource. Tags are metadata that you
-- can use to manage a resource.
--
-- 'sidewalk', 'associateAwsAccountWithPartnerAccount_sidewalk' - The Sidewalk account credentials.
newAssociateAwsAccountWithPartnerAccount ::
  -- | 'sidewalk'
  SidewalkAccountInfo ->
  AssociateAwsAccountWithPartnerAccount
newAssociateAwsAccountWithPartnerAccount pSidewalk_ =
  AssociateAwsAccountWithPartnerAccount'
    { clientRequestToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      sidewalk = pSidewalk_
    }

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
associateAwsAccountWithPartnerAccount_clientRequestToken :: Lens.Lens' AssociateAwsAccountWithPartnerAccount (Prelude.Maybe Prelude.Text)
associateAwsAccountWithPartnerAccount_clientRequestToken = Lens.lens (\AssociateAwsAccountWithPartnerAccount' {clientRequestToken} -> clientRequestToken) (\s@AssociateAwsAccountWithPartnerAccount' {} a -> s {clientRequestToken = a} :: AssociateAwsAccountWithPartnerAccount)

-- | The tags to attach to the specified resource. Tags are metadata that you
-- can use to manage a resource.
associateAwsAccountWithPartnerAccount_tags :: Lens.Lens' AssociateAwsAccountWithPartnerAccount (Prelude.Maybe [Tag])
associateAwsAccountWithPartnerAccount_tags = Lens.lens (\AssociateAwsAccountWithPartnerAccount' {tags} -> tags) (\s@AssociateAwsAccountWithPartnerAccount' {} a -> s {tags = a} :: AssociateAwsAccountWithPartnerAccount) Prelude.. Lens.mapping Lens.coerced

-- | The Sidewalk account credentials.
associateAwsAccountWithPartnerAccount_sidewalk :: Lens.Lens' AssociateAwsAccountWithPartnerAccount SidewalkAccountInfo
associateAwsAccountWithPartnerAccount_sidewalk = Lens.lens (\AssociateAwsAccountWithPartnerAccount' {sidewalk} -> sidewalk) (\s@AssociateAwsAccountWithPartnerAccount' {} a -> s {sidewalk = a} :: AssociateAwsAccountWithPartnerAccount)

instance
  Core.AWSRequest
    AssociateAwsAccountWithPartnerAccount
  where
  type
    AWSResponse
      AssociateAwsAccountWithPartnerAccount =
      AssociateAwsAccountWithPartnerAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateAwsAccountWithPartnerAccountResponse'
            Prelude.<$> (x Data..?> "Arn")
              Prelude.<*> (x Data..?> "Sidewalk")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateAwsAccountWithPartnerAccount
  where
  hashWithSalt
    _salt
    AssociateAwsAccountWithPartnerAccount' {..} =
      _salt `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    AssociateAwsAccountWithPartnerAccount
  where
  rnf AssociateAwsAccountWithPartnerAccount' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sidewalk

instance
  Data.ToHeaders
    AssociateAwsAccountWithPartnerAccount
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociateAwsAccountWithPartnerAccount
  where
  toJSON AssociateAwsAccountWithPartnerAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Sidewalk" Data..= sidewalk)
          ]
      )

instance
  Data.ToPath
    AssociateAwsAccountWithPartnerAccount
  where
  toPath = Prelude.const "/partner-accounts"

instance
  Data.ToQuery
    AssociateAwsAccountWithPartnerAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateAwsAccountWithPartnerAccountResponse' smart constructor.
data AssociateAwsAccountWithPartnerAccountResponse = AssociateAwsAccountWithPartnerAccountResponse'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk account credentials.
    sidewalk :: Prelude.Maybe SidewalkAccountInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAwsAccountWithPartnerAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associateAwsAccountWithPartnerAccountResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'sidewalk', 'associateAwsAccountWithPartnerAccountResponse_sidewalk' - The Sidewalk account credentials.
--
-- 'httpStatus', 'associateAwsAccountWithPartnerAccountResponse_httpStatus' - The response's http status code.
newAssociateAwsAccountWithPartnerAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateAwsAccountWithPartnerAccountResponse
newAssociateAwsAccountWithPartnerAccountResponse
  pHttpStatus_ =
    AssociateAwsAccountWithPartnerAccountResponse'
      { arn =
          Prelude.Nothing,
        sidewalk = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name of the resource.
associateAwsAccountWithPartnerAccountResponse_arn :: Lens.Lens' AssociateAwsAccountWithPartnerAccountResponse (Prelude.Maybe Prelude.Text)
associateAwsAccountWithPartnerAccountResponse_arn = Lens.lens (\AssociateAwsAccountWithPartnerAccountResponse' {arn} -> arn) (\s@AssociateAwsAccountWithPartnerAccountResponse' {} a -> s {arn = a} :: AssociateAwsAccountWithPartnerAccountResponse)

-- | The Sidewalk account credentials.
associateAwsAccountWithPartnerAccountResponse_sidewalk :: Lens.Lens' AssociateAwsAccountWithPartnerAccountResponse (Prelude.Maybe SidewalkAccountInfo)
associateAwsAccountWithPartnerAccountResponse_sidewalk = Lens.lens (\AssociateAwsAccountWithPartnerAccountResponse' {sidewalk} -> sidewalk) (\s@AssociateAwsAccountWithPartnerAccountResponse' {} a -> s {sidewalk = a} :: AssociateAwsAccountWithPartnerAccountResponse)

-- | The response's http status code.
associateAwsAccountWithPartnerAccountResponse_httpStatus :: Lens.Lens' AssociateAwsAccountWithPartnerAccountResponse Prelude.Int
associateAwsAccountWithPartnerAccountResponse_httpStatus = Lens.lens (\AssociateAwsAccountWithPartnerAccountResponse' {httpStatus} -> httpStatus) (\s@AssociateAwsAccountWithPartnerAccountResponse' {} a -> s {httpStatus = a} :: AssociateAwsAccountWithPartnerAccountResponse)

instance
  Prelude.NFData
    AssociateAwsAccountWithPartnerAccountResponse
  where
  rnf
    AssociateAwsAccountWithPartnerAccountResponse' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf sidewalk
        `Prelude.seq` Prelude.rnf httpStatus
