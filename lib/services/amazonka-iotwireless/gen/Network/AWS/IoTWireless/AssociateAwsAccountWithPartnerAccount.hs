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
-- Module      : Network.AWS.IoTWireless.AssociateAwsAccountWithPartnerAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a partner account with your AWS account.
module Network.AWS.IoTWireless.AssociateAwsAccountWithPartnerAccount
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
    associateAwsAccountWithPartnerAccountResponse_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_arn,
    associateAwsAccountWithPartnerAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateAwsAccountWithPartnerAccountResponse'
            Prelude.<$> (x Core..?> "Sidewalk")
              Prelude.<*> (x Core..?> "Arn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateAwsAccountWithPartnerAccount

instance
  Prelude.NFData
    AssociateAwsAccountWithPartnerAccount

instance
  Core.ToHeaders
    AssociateAwsAccountWithPartnerAccount
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    AssociateAwsAccountWithPartnerAccount
  where
  toJSON AssociateAwsAccountWithPartnerAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Sidewalk" Core..= sidewalk)
          ]
      )

instance
  Core.ToPath
    AssociateAwsAccountWithPartnerAccount
  where
  toPath = Prelude.const "/partner-accounts"

instance
  Core.ToQuery
    AssociateAwsAccountWithPartnerAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateAwsAccountWithPartnerAccountResponse' smart constructor.
data AssociateAwsAccountWithPartnerAccountResponse = AssociateAwsAccountWithPartnerAccountResponse'
  { -- | The Sidewalk account credentials.
    sidewalk :: Prelude.Maybe SidewalkAccountInfo,
    -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
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
-- 'sidewalk', 'associateAwsAccountWithPartnerAccountResponse_sidewalk' - The Sidewalk account credentials.
--
-- 'arn', 'associateAwsAccountWithPartnerAccountResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'httpStatus', 'associateAwsAccountWithPartnerAccountResponse_httpStatus' - The response's http status code.
newAssociateAwsAccountWithPartnerAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateAwsAccountWithPartnerAccountResponse
newAssociateAwsAccountWithPartnerAccountResponse
  pHttpStatus_ =
    AssociateAwsAccountWithPartnerAccountResponse'
      { sidewalk =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Sidewalk account credentials.
associateAwsAccountWithPartnerAccountResponse_sidewalk :: Lens.Lens' AssociateAwsAccountWithPartnerAccountResponse (Prelude.Maybe SidewalkAccountInfo)
associateAwsAccountWithPartnerAccountResponse_sidewalk = Lens.lens (\AssociateAwsAccountWithPartnerAccountResponse' {sidewalk} -> sidewalk) (\s@AssociateAwsAccountWithPartnerAccountResponse' {} a -> s {sidewalk = a} :: AssociateAwsAccountWithPartnerAccountResponse)

-- | The Amazon Resource Name of the resource.
associateAwsAccountWithPartnerAccountResponse_arn :: Lens.Lens' AssociateAwsAccountWithPartnerAccountResponse (Prelude.Maybe Prelude.Text)
associateAwsAccountWithPartnerAccountResponse_arn = Lens.lens (\AssociateAwsAccountWithPartnerAccountResponse' {arn} -> arn) (\s@AssociateAwsAccountWithPartnerAccountResponse' {} a -> s {arn = a} :: AssociateAwsAccountWithPartnerAccountResponse)

-- | The response's http status code.
associateAwsAccountWithPartnerAccountResponse_httpStatus :: Lens.Lens' AssociateAwsAccountWithPartnerAccountResponse Prelude.Int
associateAwsAccountWithPartnerAccountResponse_httpStatus = Lens.lens (\AssociateAwsAccountWithPartnerAccountResponse' {httpStatus} -> httpStatus) (\s@AssociateAwsAccountWithPartnerAccountResponse' {} a -> s {httpStatus = a} :: AssociateAwsAccountWithPartnerAccountResponse)

instance
  Prelude.NFData
    AssociateAwsAccountWithPartnerAccountResponse
