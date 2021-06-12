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
-- Module      : Network.AWS.APIGateway.GetVpcLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a specified VPC link under the caller\'s account in a region.
module Network.AWS.APIGateway.GetVpcLink
  ( -- * Creating a Request
    GetVpcLink (..),
    newGetVpcLink,

    -- * Request Lenses
    getVpcLink_vpcLinkId,

    -- * Destructuring the Response
    VpcLink (..),
    newVpcLink,

    -- * Response Lenses
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets a specified VPC link under the caller\'s account in a region.
--
-- /See:/ 'newGetVpcLink' smart constructor.
data GetVpcLink = GetVpcLink'
  { -- | [Required] The identifier of the VpcLink. It is used in an Integration
    -- to reference this VpcLink.
    vpcLinkId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcLinkId', 'getVpcLink_vpcLinkId' - [Required] The identifier of the VpcLink. It is used in an Integration
-- to reference this VpcLink.
newGetVpcLink ::
  -- | 'vpcLinkId'
  Core.Text ->
  GetVpcLink
newGetVpcLink pVpcLinkId_ =
  GetVpcLink' {vpcLinkId = pVpcLinkId_}

-- | [Required] The identifier of the VpcLink. It is used in an Integration
-- to reference this VpcLink.
getVpcLink_vpcLinkId :: Lens.Lens' GetVpcLink Core.Text
getVpcLink_vpcLinkId = Lens.lens (\GetVpcLink' {vpcLinkId} -> vpcLinkId) (\s@GetVpcLink' {} a -> s {vpcLinkId = a} :: GetVpcLink)

instance Core.AWSRequest GetVpcLink where
  type AWSResponse GetVpcLink = VpcLink
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetVpcLink

instance Core.NFData GetVpcLink

instance Core.ToHeaders GetVpcLink where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetVpcLink where
  toPath GetVpcLink' {..} =
    Core.mconcat ["/vpclinks/", Core.toBS vpcLinkId]

instance Core.ToQuery GetVpcLink where
  toQuery = Core.const Core.mempty
