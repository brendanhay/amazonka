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
-- Module      : Network.AWS.APIGateway.CreateUsagePlanKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan key for adding an existing API key to a usage plan.
module Network.AWS.APIGateway.CreateUsagePlanKey
  ( -- * Creating a Request
    CreateUsagePlanKey (..),
    newCreateUsagePlanKey,

    -- * Request Lenses
    createUsagePlanKey_usagePlanId,
    createUsagePlanKey_keyId,
    createUsagePlanKey_keyType,

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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST request to create a usage plan key for adding an existing API
-- key to a usage plan.
--
-- /See:/ 'newCreateUsagePlanKey' smart constructor.
data CreateUsagePlanKey = CreateUsagePlanKey'
  { -- | [Required] The Id of the UsagePlan resource representing the usage plan
    -- containing the to-be-created UsagePlanKey resource representing a plan
    -- customer.
    usagePlanId :: Core.Text,
    -- | [Required] The identifier of a UsagePlanKey resource for a plan
    -- customer.
    keyId :: Core.Text,
    -- | [Required] The type of a UsagePlanKey resource for a plan customer.
    keyType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUsagePlanKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'createUsagePlanKey_usagePlanId' - [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-created UsagePlanKey resource representing a plan
-- customer.
--
-- 'keyId', 'createUsagePlanKey_keyId' - [Required] The identifier of a UsagePlanKey resource for a plan
-- customer.
--
-- 'keyType', 'createUsagePlanKey_keyType' - [Required] The type of a UsagePlanKey resource for a plan customer.
newCreateUsagePlanKey ::
  -- | 'usagePlanId'
  Core.Text ->
  -- | 'keyId'
  Core.Text ->
  -- | 'keyType'
  Core.Text ->
  CreateUsagePlanKey
newCreateUsagePlanKey pUsagePlanId_ pKeyId_ pKeyType_ =
  CreateUsagePlanKey'
    { usagePlanId = pUsagePlanId_,
      keyId = pKeyId_,
      keyType = pKeyType_
    }

-- | [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-created UsagePlanKey resource representing a plan
-- customer.
createUsagePlanKey_usagePlanId :: Lens.Lens' CreateUsagePlanKey Core.Text
createUsagePlanKey_usagePlanId = Lens.lens (\CreateUsagePlanKey' {usagePlanId} -> usagePlanId) (\s@CreateUsagePlanKey' {} a -> s {usagePlanId = a} :: CreateUsagePlanKey)

-- | [Required] The identifier of a UsagePlanKey resource for a plan
-- customer.
createUsagePlanKey_keyId :: Lens.Lens' CreateUsagePlanKey Core.Text
createUsagePlanKey_keyId = Lens.lens (\CreateUsagePlanKey' {keyId} -> keyId) (\s@CreateUsagePlanKey' {} a -> s {keyId = a} :: CreateUsagePlanKey)

-- | [Required] The type of a UsagePlanKey resource for a plan customer.
createUsagePlanKey_keyType :: Lens.Lens' CreateUsagePlanKey Core.Text
createUsagePlanKey_keyType = Lens.lens (\CreateUsagePlanKey' {keyType} -> keyType) (\s@CreateUsagePlanKey' {} a -> s {keyType = a} :: CreateUsagePlanKey)

instance Core.AWSRequest CreateUsagePlanKey where
  type AWSResponse CreateUsagePlanKey = UsagePlanKey
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateUsagePlanKey

instance Core.NFData CreateUsagePlanKey

instance Core.ToHeaders CreateUsagePlanKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUsagePlanKey where
  toJSON CreateUsagePlanKey' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("keyId" Core..= keyId),
            Core.Just ("keyType" Core..= keyType)
          ]
      )

instance Core.ToPath CreateUsagePlanKey where
  toPath CreateUsagePlanKey' {..} =
    Core.mconcat
      ["/usageplans/", Core.toBS usagePlanId, "/keys"]

instance Core.ToQuery CreateUsagePlanKey where
  toQuery = Core.const Core.mempty
