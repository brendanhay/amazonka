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
-- Module      : Amazonka.APIGateway.GetUsagePlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan of a given plan identifier.
module Amazonka.APIGateway.GetUsagePlan
  ( -- * Creating a Request
    GetUsagePlan (..),
    newGetUsagePlan,

    -- * Request Lenses
    getUsagePlan_usagePlanId,

    -- * Destructuring the Response
    UsagePlan (..),
    newUsagePlan,

    -- * Response Lenses
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The GET request to get a usage plan of a given plan identifier.
--
-- /See:/ 'newGetUsagePlan' smart constructor.
data GetUsagePlan = GetUsagePlan'
  { -- | The identifier of the UsagePlan resource to be retrieved.
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
-- 'usagePlanId', 'getUsagePlan_usagePlanId' - The identifier of the UsagePlan resource to be retrieved.
newGetUsagePlan ::
  -- | 'usagePlanId'
  Prelude.Text ->
  GetUsagePlan
newGetUsagePlan pUsagePlanId_ =
  GetUsagePlan' {usagePlanId = pUsagePlanId_}

-- | The identifier of the UsagePlan resource to be retrieved.
getUsagePlan_usagePlanId :: Lens.Lens' GetUsagePlan Prelude.Text
getUsagePlan_usagePlanId = Lens.lens (\GetUsagePlan' {usagePlanId} -> usagePlanId) (\s@GetUsagePlan' {} a -> s {usagePlanId = a} :: GetUsagePlan)

instance Core.AWSRequest GetUsagePlan where
  type AWSResponse GetUsagePlan = UsagePlan
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetUsagePlan where
  hashWithSalt _salt GetUsagePlan' {..} =
    _salt `Prelude.hashWithSalt` usagePlanId

instance Prelude.NFData GetUsagePlan where
  rnf GetUsagePlan' {..} = Prelude.rnf usagePlanId

instance Data.ToHeaders GetUsagePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetUsagePlan where
  toPath GetUsagePlan' {..} =
    Prelude.mconcat
      ["/usageplans/", Data.toBS usagePlanId]

instance Data.ToQuery GetUsagePlan where
  toQuery = Prelude.const Prelude.mempty
