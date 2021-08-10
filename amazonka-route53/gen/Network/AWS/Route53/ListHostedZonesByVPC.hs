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
-- Module      : Network.AWS.Route53.ListHostedZonesByVPC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the private hosted zones that a specified VPC is associated
-- with, regardless of which AWS account or AWS service owns the hosted
-- zones. The @HostedZoneOwner@ structure in the response contains one of
-- the following values:
--
-- -   An @OwningAccount@ element, which contains the account number of
--     either the current AWS account or another AWS account. Some
--     services, such as AWS Cloud Map, create hosted zones using the
--     current account.
--
-- -   An @OwningService@ element, which identifies the AWS service that
--     created and owns the hosted zone. For example, if a hosted zone was
--     created by Amazon Elastic File System (Amazon EFS), the value of
--     @Owner@ is @efs.amazonaws.com@.
module Network.AWS.Route53.ListHostedZonesByVPC
  ( -- * Creating a Request
    ListHostedZonesByVPC (..),
    newListHostedZonesByVPC,

    -- * Request Lenses
    listHostedZonesByVPC_nextToken,
    listHostedZonesByVPC_maxItems,
    listHostedZonesByVPC_vPCId,
    listHostedZonesByVPC_vPCRegion,

    -- * Destructuring the Response
    ListHostedZonesByVPCResponse (..),
    newListHostedZonesByVPCResponse,

    -- * Response Lenses
    listHostedZonesByVPCResponse_nextToken,
    listHostedZonesByVPCResponse_httpStatus,
    listHostedZonesByVPCResponse_hostedZoneSummaries,
    listHostedZonesByVPCResponse_maxItems,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | Lists all the private hosted zones that a specified VPC is associated
-- with, regardless of which AWS account created the hosted zones.
--
-- /See:/ 'newListHostedZonesByVPC' smart constructor.
data ListHostedZonesByVPC = ListHostedZonesByVPC'
  { -- | If the previous response included a @NextToken@ element, the specified
    -- VPC is associated with more hosted zones. To get more hosted zones,
    -- submit another @ListHostedZonesByVPC@ request.
    --
    -- For the value of @NextToken@, specify the value of @NextToken@ from the
    -- previous response.
    --
    -- If the previous response didn\'t include a @NextToken@ element, there
    -- are no more hosted zones to get.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of hosted zones that you want Amazon Route
    -- 53 to return. If the specified VPC is associated with more than
    -- @MaxItems@ hosted zones, the response includes a @NextToken@ element.
    -- @NextToken@ contains an encrypted token that identifies the first hosted
    -- zone that Route 53 will return if you submit another request.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon VPC that you want to list hosted zones for.
    vPCId :: Prelude.Text,
    -- | For the Amazon VPC that you specified for @VPCId@, the AWS Region that
    -- you created the VPC in.
    vPCRegion :: VPCRegion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostedZonesByVPC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHostedZonesByVPC_nextToken' - If the previous response included a @NextToken@ element, the specified
-- VPC is associated with more hosted zones. To get more hosted zones,
-- submit another @ListHostedZonesByVPC@ request.
--
-- For the value of @NextToken@, specify the value of @NextToken@ from the
-- previous response.
--
-- If the previous response didn\'t include a @NextToken@ element, there
-- are no more hosted zones to get.
--
-- 'maxItems', 'listHostedZonesByVPC_maxItems' - (Optional) The maximum number of hosted zones that you want Amazon Route
-- 53 to return. If the specified VPC is associated with more than
-- @MaxItems@ hosted zones, the response includes a @NextToken@ element.
-- @NextToken@ contains an encrypted token that identifies the first hosted
-- zone that Route 53 will return if you submit another request.
--
-- 'vPCId', 'listHostedZonesByVPC_vPCId' - The ID of the Amazon VPC that you want to list hosted zones for.
--
-- 'vPCRegion', 'listHostedZonesByVPC_vPCRegion' - For the Amazon VPC that you specified for @VPCId@, the AWS Region that
-- you created the VPC in.
newListHostedZonesByVPC ::
  -- | 'vPCId'
  Prelude.Text ->
  -- | 'vPCRegion'
  VPCRegion ->
  ListHostedZonesByVPC
newListHostedZonesByVPC pVPCId_ pVPCRegion_ =
  ListHostedZonesByVPC'
    { nextToken = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      vPCId = pVPCId_,
      vPCRegion = pVPCRegion_
    }

-- | If the previous response included a @NextToken@ element, the specified
-- VPC is associated with more hosted zones. To get more hosted zones,
-- submit another @ListHostedZonesByVPC@ request.
--
-- For the value of @NextToken@, specify the value of @NextToken@ from the
-- previous response.
--
-- If the previous response didn\'t include a @NextToken@ element, there
-- are no more hosted zones to get.
listHostedZonesByVPC_nextToken :: Lens.Lens' ListHostedZonesByVPC (Prelude.Maybe Prelude.Text)
listHostedZonesByVPC_nextToken = Lens.lens (\ListHostedZonesByVPC' {nextToken} -> nextToken) (\s@ListHostedZonesByVPC' {} a -> s {nextToken = a} :: ListHostedZonesByVPC)

-- | (Optional) The maximum number of hosted zones that you want Amazon Route
-- 53 to return. If the specified VPC is associated with more than
-- @MaxItems@ hosted zones, the response includes a @NextToken@ element.
-- @NextToken@ contains an encrypted token that identifies the first hosted
-- zone that Route 53 will return if you submit another request.
listHostedZonesByVPC_maxItems :: Lens.Lens' ListHostedZonesByVPC (Prelude.Maybe Prelude.Text)
listHostedZonesByVPC_maxItems = Lens.lens (\ListHostedZonesByVPC' {maxItems} -> maxItems) (\s@ListHostedZonesByVPC' {} a -> s {maxItems = a} :: ListHostedZonesByVPC)

-- | The ID of the Amazon VPC that you want to list hosted zones for.
listHostedZonesByVPC_vPCId :: Lens.Lens' ListHostedZonesByVPC Prelude.Text
listHostedZonesByVPC_vPCId = Lens.lens (\ListHostedZonesByVPC' {vPCId} -> vPCId) (\s@ListHostedZonesByVPC' {} a -> s {vPCId = a} :: ListHostedZonesByVPC)

-- | For the Amazon VPC that you specified for @VPCId@, the AWS Region that
-- you created the VPC in.
listHostedZonesByVPC_vPCRegion :: Lens.Lens' ListHostedZonesByVPC VPCRegion
listHostedZonesByVPC_vPCRegion = Lens.lens (\ListHostedZonesByVPC' {vPCRegion} -> vPCRegion) (\s@ListHostedZonesByVPC' {} a -> s {vPCRegion = a} :: ListHostedZonesByVPC)

instance Core.AWSRequest ListHostedZonesByVPC where
  type
    AWSResponse ListHostedZonesByVPC =
      ListHostedZonesByVPCResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListHostedZonesByVPCResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "HostedZoneSummaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "HostedZoneSummary"
                        )
            Prelude.<*> (x Core..@ "MaxItems")
      )

instance Prelude.Hashable ListHostedZonesByVPC

instance Prelude.NFData ListHostedZonesByVPC

instance Core.ToHeaders ListHostedZonesByVPC where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListHostedZonesByVPC where
  toPath = Prelude.const "/2013-04-01/hostedzonesbyvpc"

instance Core.ToQuery ListHostedZonesByVPC where
  toQuery ListHostedZonesByVPC' {..} =
    Prelude.mconcat
      [ "nexttoken" Core.=: nextToken,
        "maxitems" Core.=: maxItems,
        "vpcid" Core.=: vPCId,
        "vpcregion" Core.=: vPCRegion
      ]

-- | /See:/ 'newListHostedZonesByVPCResponse' smart constructor.
data ListHostedZonesByVPCResponse = ListHostedZonesByVPCResponse'
  { -- | The value that you specified for @NextToken@ in the most recent
    -- @ListHostedZonesByVPC@ request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @HostedZoneSummary@ element for each hosted
    -- zone that the specified Amazon VPC is associated with. Each
    -- @HostedZoneSummary@ element contains the hosted zone name and ID, and
    -- information about who owns the hosted zone.
    hostedZoneSummaries :: [HostedZoneSummary],
    -- | The value that you specified for @MaxItems@ in the most recent
    -- @ListHostedZonesByVPC@ request.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostedZonesByVPCResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHostedZonesByVPCResponse_nextToken' - The value that you specified for @NextToken@ in the most recent
-- @ListHostedZonesByVPC@ request.
--
-- 'httpStatus', 'listHostedZonesByVPCResponse_httpStatus' - The response's http status code.
--
-- 'hostedZoneSummaries', 'listHostedZonesByVPCResponse_hostedZoneSummaries' - A list that contains one @HostedZoneSummary@ element for each hosted
-- zone that the specified Amazon VPC is associated with. Each
-- @HostedZoneSummary@ element contains the hosted zone name and ID, and
-- information about who owns the hosted zone.
--
-- 'maxItems', 'listHostedZonesByVPCResponse_maxItems' - The value that you specified for @MaxItems@ in the most recent
-- @ListHostedZonesByVPC@ request.
newListHostedZonesByVPCResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'maxItems'
  Prelude.Text ->
  ListHostedZonesByVPCResponse
newListHostedZonesByVPCResponse
  pHttpStatus_
  pMaxItems_ =
    ListHostedZonesByVPCResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hostedZoneSummaries = Prelude.mempty,
        maxItems = pMaxItems_
      }

-- | The value that you specified for @NextToken@ in the most recent
-- @ListHostedZonesByVPC@ request.
listHostedZonesByVPCResponse_nextToken :: Lens.Lens' ListHostedZonesByVPCResponse (Prelude.Maybe Prelude.Text)
listHostedZonesByVPCResponse_nextToken = Lens.lens (\ListHostedZonesByVPCResponse' {nextToken} -> nextToken) (\s@ListHostedZonesByVPCResponse' {} a -> s {nextToken = a} :: ListHostedZonesByVPCResponse)

-- | The response's http status code.
listHostedZonesByVPCResponse_httpStatus :: Lens.Lens' ListHostedZonesByVPCResponse Prelude.Int
listHostedZonesByVPCResponse_httpStatus = Lens.lens (\ListHostedZonesByVPCResponse' {httpStatus} -> httpStatus) (\s@ListHostedZonesByVPCResponse' {} a -> s {httpStatus = a} :: ListHostedZonesByVPCResponse)

-- | A list that contains one @HostedZoneSummary@ element for each hosted
-- zone that the specified Amazon VPC is associated with. Each
-- @HostedZoneSummary@ element contains the hosted zone name and ID, and
-- information about who owns the hosted zone.
listHostedZonesByVPCResponse_hostedZoneSummaries :: Lens.Lens' ListHostedZonesByVPCResponse [HostedZoneSummary]
listHostedZonesByVPCResponse_hostedZoneSummaries = Lens.lens (\ListHostedZonesByVPCResponse' {hostedZoneSummaries} -> hostedZoneSummaries) (\s@ListHostedZonesByVPCResponse' {} a -> s {hostedZoneSummaries = a} :: ListHostedZonesByVPCResponse) Prelude.. Lens._Coerce

-- | The value that you specified for @MaxItems@ in the most recent
-- @ListHostedZonesByVPC@ request.
listHostedZonesByVPCResponse_maxItems :: Lens.Lens' ListHostedZonesByVPCResponse Prelude.Text
listHostedZonesByVPCResponse_maxItems = Lens.lens (\ListHostedZonesByVPCResponse' {maxItems} -> maxItems) (\s@ListHostedZonesByVPCResponse' {} a -> s {maxItems = a} :: ListHostedZonesByVPCResponse)

instance Prelude.NFData ListHostedZonesByVPCResponse
