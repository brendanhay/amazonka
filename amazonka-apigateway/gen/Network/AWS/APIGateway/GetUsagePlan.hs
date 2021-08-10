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
-- Module      : Network.AWS.APIGateway.GetUsagePlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan of a given plan identifier.
module Network.AWS.APIGateway.GetUsagePlan
  ( -- * Creating a Request
    GetUsagePlan (..),
    newGetUsagePlan,

    -- * Request Lenses
    getUsagePlan_usagePlanId,

    -- * Destructuring the Response
    UsagePlan (..),
    newUsagePlan,

    -- * Response Lenses
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get a usage plan of a given plan identifier.
--
-- /See:/ 'newGetUsagePlan' smart constructor.
data GetUsagePlan = GetUsagePlan'
  { -- | [Required] The identifier of the UsagePlan resource to be retrieved.
    usagePlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'getUsagePlan_usagePlanId' - [Required] The identifier of the UsagePlan resource to be retrieved.
newGetUsagePlan ::
  -- | 'usagePlanId'
  Prelude.Text ->
  GetUsagePlan
newGetUsagePlan pUsagePlanId_ =
  GetUsagePlan' {usagePlanId = pUsagePlanId_}

-- | [Required] The identifier of the UsagePlan resource to be retrieved.
getUsagePlan_usagePlanId :: Lens.Lens' GetUsagePlan Prelude.Text
getUsagePlan_usagePlanId = Lens.lens (\GetUsagePlan' {usagePlanId} -> usagePlanId) (\s@GetUsagePlan' {} a -> s {usagePlanId = a} :: GetUsagePlan)

instance Core.AWSRequest GetUsagePlan where
  type AWSResponse GetUsagePlan = UsagePlan
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetUsagePlan

instance Prelude.NFData GetUsagePlan

instance Core.ToHeaders GetUsagePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetUsagePlan where
  toPath GetUsagePlan' {..} =
    Prelude.mconcat
      ["/usageplans/", Core.toBS usagePlanId]

instance Core.ToQuery GetUsagePlan where
  toQuery = Prelude.const Prelude.mempty
