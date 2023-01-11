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
-- Module      : Amazonka.APIGateway.GetVpcLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a specified VPC link under the caller\'s account in a region.
module Amazonka.APIGateway.GetVpcLink
  ( -- * Creating a Request
    GetVpcLink (..),
    newGetVpcLink,

    -- * Request Lenses
    getVpcLink_vpcLinkId,

    -- * Destructuring the Response
    VpcLink (..),
    newVpcLink,

    -- * Response Lenses
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets a specified VPC link under the caller\'s account in a region.
--
-- /See:/ 'newGetVpcLink' smart constructor.
data GetVpcLink = GetVpcLink'
  { -- | The identifier of the VpcLink. It is used in an Integration to reference
    -- this VpcLink.
    vpcLinkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcLinkId', 'getVpcLink_vpcLinkId' - The identifier of the VpcLink. It is used in an Integration to reference
-- this VpcLink.
newGetVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  GetVpcLink
newGetVpcLink pVpcLinkId_ =
  GetVpcLink' {vpcLinkId = pVpcLinkId_}

-- | The identifier of the VpcLink. It is used in an Integration to reference
-- this VpcLink.
getVpcLink_vpcLinkId :: Lens.Lens' GetVpcLink Prelude.Text
getVpcLink_vpcLinkId = Lens.lens (\GetVpcLink' {vpcLinkId} -> vpcLinkId) (\s@GetVpcLink' {} a -> s {vpcLinkId = a} :: GetVpcLink)

instance Core.AWSRequest GetVpcLink where
  type AWSResponse GetVpcLink = VpcLink
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetVpcLink where
  hashWithSalt _salt GetVpcLink' {..} =
    _salt `Prelude.hashWithSalt` vpcLinkId

instance Prelude.NFData GetVpcLink where
  rnf GetVpcLink' {..} = Prelude.rnf vpcLinkId

instance Data.ToHeaders GetVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetVpcLink where
  toPath GetVpcLink' {..} =
    Prelude.mconcat ["/vpclinks/", Data.toBS vpcLinkId]

instance Data.ToQuery GetVpcLink where
  toQuery = Prelude.const Prelude.mempty
