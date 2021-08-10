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
-- Module      : Network.AWS.APIGateway.GetUsagePlanKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan key of a given key identifier.
module Network.AWS.APIGateway.GetUsagePlanKey
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
    usagePlanKey_value,
    usagePlanKey_type,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get a usage plan key of a given key identifier.
--
-- /See:/ 'newGetUsagePlanKey' smart constructor.
data GetUsagePlanKey = GetUsagePlanKey'
  { -- | [Required] The Id of the UsagePlan resource representing the usage plan
    -- containing the to-be-retrieved UsagePlanKey resource representing a plan
    -- customer.
    usagePlanId :: Prelude.Text,
    -- | [Required] The key Id of the to-be-retrieved UsagePlanKey resource
    -- representing a plan customer.
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
-- 'usagePlanId', 'getUsagePlanKey_usagePlanId' - [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-retrieved UsagePlanKey resource representing a plan
-- customer.
--
-- 'keyId', 'getUsagePlanKey_keyId' - [Required] The key Id of the to-be-retrieved UsagePlanKey resource
-- representing a plan customer.
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

-- | [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-retrieved UsagePlanKey resource representing a plan
-- customer.
getUsagePlanKey_usagePlanId :: Lens.Lens' GetUsagePlanKey Prelude.Text
getUsagePlanKey_usagePlanId = Lens.lens (\GetUsagePlanKey' {usagePlanId} -> usagePlanId) (\s@GetUsagePlanKey' {} a -> s {usagePlanId = a} :: GetUsagePlanKey)

-- | [Required] The key Id of the to-be-retrieved UsagePlanKey resource
-- representing a plan customer.
getUsagePlanKey_keyId :: Lens.Lens' GetUsagePlanKey Prelude.Text
getUsagePlanKey_keyId = Lens.lens (\GetUsagePlanKey' {keyId} -> keyId) (\s@GetUsagePlanKey' {} a -> s {keyId = a} :: GetUsagePlanKey)

instance Core.AWSRequest GetUsagePlanKey where
  type AWSResponse GetUsagePlanKey = UsagePlanKey
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetUsagePlanKey

instance Prelude.NFData GetUsagePlanKey

instance Core.ToHeaders GetUsagePlanKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetUsagePlanKey where
  toPath GetUsagePlanKey' {..} =
    Prelude.mconcat
      [ "/usageplans/",
        Core.toBS usagePlanId,
        "/keys/",
        Core.toBS keyId
      ]

instance Core.ToQuery GetUsagePlanKey where
  toQuery = Prelude.const Prelude.mempty
