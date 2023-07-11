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
-- Module      : Amazonka.APIGateway.GetUsagePlanKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan key of a given key identifier.
module Amazonka.APIGateway.GetUsagePlanKey
  ( -- * Creating a Request
    GetUsagePlanKey (..),
    newGetUsagePlanKey,

    -- * Request Lenses
    getUsagePlanKey_usagePlanId,
    getUsagePlanKey_keyId,

    -- * Destructuring the Response
    UsagePlanKey (..),
    newUsagePlanKey,

    -- * Response Lenses
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_value,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The GET request to get a usage plan key of a given key identifier.
--
-- /See:/ 'newGetUsagePlanKey' smart constructor.
data GetUsagePlanKey = GetUsagePlanKey'
  { -- | The Id of the UsagePlan resource representing the usage plan containing
    -- the to-be-retrieved UsagePlanKey resource representing a plan customer.
    usagePlanId :: Prelude.Text,
    -- | The key Id of the to-be-retrieved UsagePlanKey resource representing a
    -- plan customer.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsagePlanKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'getUsagePlanKey_usagePlanId' - The Id of the UsagePlan resource representing the usage plan containing
-- the to-be-retrieved UsagePlanKey resource representing a plan customer.
--
-- 'keyId', 'getUsagePlanKey_keyId' - The key Id of the to-be-retrieved UsagePlanKey resource representing a
-- plan customer.
newGetUsagePlanKey ::
  -- | 'usagePlanId'
  Prelude.Text ->
  -- | 'keyId'
  Prelude.Text ->
  GetUsagePlanKey
newGetUsagePlanKey pUsagePlanId_ pKeyId_ =
  GetUsagePlanKey'
    { usagePlanId = pUsagePlanId_,
      keyId = pKeyId_
    }

-- | The Id of the UsagePlan resource representing the usage plan containing
-- the to-be-retrieved UsagePlanKey resource representing a plan customer.
getUsagePlanKey_usagePlanId :: Lens.Lens' GetUsagePlanKey Prelude.Text
getUsagePlanKey_usagePlanId = Lens.lens (\GetUsagePlanKey' {usagePlanId} -> usagePlanId) (\s@GetUsagePlanKey' {} a -> s {usagePlanId = a} :: GetUsagePlanKey)

-- | The key Id of the to-be-retrieved UsagePlanKey resource representing a
-- plan customer.
getUsagePlanKey_keyId :: Lens.Lens' GetUsagePlanKey Prelude.Text
getUsagePlanKey_keyId = Lens.lens (\GetUsagePlanKey' {keyId} -> keyId) (\s@GetUsagePlanKey' {} a -> s {keyId = a} :: GetUsagePlanKey)

instance Core.AWSRequest GetUsagePlanKey where
  type AWSResponse GetUsagePlanKey = UsagePlanKey
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetUsagePlanKey where
  hashWithSalt _salt GetUsagePlanKey' {..} =
    _salt
      `Prelude.hashWithSalt` usagePlanId
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData GetUsagePlanKey where
  rnf GetUsagePlanKey' {..} =
    Prelude.rnf usagePlanId
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders GetUsagePlanKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetUsagePlanKey where
  toPath GetUsagePlanKey' {..} =
    Prelude.mconcat
      [ "/usageplans/",
        Data.toBS usagePlanId,
        "/keys/",
        Data.toBS keyId
      ]

instance Data.ToQuery GetUsagePlanKey where
  toQuery = Prelude.const Prelude.mempty
