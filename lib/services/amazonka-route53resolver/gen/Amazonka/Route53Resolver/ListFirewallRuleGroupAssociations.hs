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
-- Module      : Amazonka.Route53Resolver.ListFirewallRuleGroupAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the firewall rule group associations that you have defined.
-- Each association enables DNS filtering for a VPC with one rule group.
--
-- A single call might return only a partial list of the associations. For
-- information, see @MaxResults@.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListFirewallRuleGroupAssociations
  ( -- * Creating a Request
    ListFirewallRuleGroupAssociations (..),
    newListFirewallRuleGroupAssociations,

    -- * Request Lenses
    listFirewallRuleGroupAssociations_firewallRuleGroupId,
    listFirewallRuleGroupAssociations_maxResults,
    listFirewallRuleGroupAssociations_nextToken,
    listFirewallRuleGroupAssociations_priority,
    listFirewallRuleGroupAssociations_status,
    listFirewallRuleGroupAssociations_vpcId,

    -- * Destructuring the Response
    ListFirewallRuleGroupAssociationsResponse (..),
    newListFirewallRuleGroupAssociationsResponse,

    -- * Response Lenses
    listFirewallRuleGroupAssociationsResponse_firewallRuleGroupAssociations,
    listFirewallRuleGroupAssociationsResponse_nextToken,
    listFirewallRuleGroupAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListFirewallRuleGroupAssociations' smart constructor.
data ListFirewallRuleGroupAssociations = ListFirewallRuleGroupAssociations'
  { -- | The unique identifier of the firewall rule group that you want to
    -- retrieve the associations for. Leave this blank to retrieve associations
    -- for any rule group.
    firewallRuleGroupId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Resolver to return for this
    -- request. If more objects are available, in the response, Resolver
    -- provides a @NextToken@ value that you can use in a subsequent call to
    -- get the next batch of objects.
    --
    -- If you don\'t specify a value for @MaxResults@, Resolver returns up to
    -- 100 objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first call to this list request, omit this value.
    --
    -- When you request a list of objects, Resolver returns at most the number
    -- of objects specified in @MaxResults@. If more objects are available for
    -- retrieval, Resolver returns a @NextToken@ value in the response. To
    -- retrieve the next batch of objects, use the token that was returned for
    -- the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The setting that determines the processing order of the rule group among
    -- the rule groups that are associated with a single VPC. DNS Firewall
    -- filters VPC traffic starting from the rule group with the lowest numeric
    -- priority setting.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The association @Status@ setting that you want DNS Firewall to filter on
    -- for the list. If you don\'t specify this, then DNS Firewall returns all
    -- associations, regardless of status.
    status :: Prelude.Maybe FirewallRuleGroupAssociationStatus,
    -- | The unique identifier of the VPC that you want to retrieve the
    -- associations for. Leave this blank to retrieve associations for any VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallRuleGroupAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupId', 'listFirewallRuleGroupAssociations_firewallRuleGroupId' - The unique identifier of the firewall rule group that you want to
-- retrieve the associations for. Leave this blank to retrieve associations
-- for any rule group.
--
-- 'maxResults', 'listFirewallRuleGroupAssociations_maxResults' - The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
--
-- 'nextToken', 'listFirewallRuleGroupAssociations_nextToken' - For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
--
-- 'priority', 'listFirewallRuleGroupAssociations_priority' - The setting that determines the processing order of the rule group among
-- the rule groups that are associated with a single VPC. DNS Firewall
-- filters VPC traffic starting from the rule group with the lowest numeric
-- priority setting.
--
-- 'status', 'listFirewallRuleGroupAssociations_status' - The association @Status@ setting that you want DNS Firewall to filter on
-- for the list. If you don\'t specify this, then DNS Firewall returns all
-- associations, regardless of status.
--
-- 'vpcId', 'listFirewallRuleGroupAssociations_vpcId' - The unique identifier of the VPC that you want to retrieve the
-- associations for. Leave this blank to retrieve associations for any VPC.
newListFirewallRuleGroupAssociations ::
  ListFirewallRuleGroupAssociations
newListFirewallRuleGroupAssociations =
  ListFirewallRuleGroupAssociations'
    { firewallRuleGroupId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      priority = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The unique identifier of the firewall rule group that you want to
-- retrieve the associations for. Leave this blank to retrieve associations
-- for any rule group.
listFirewallRuleGroupAssociations_firewallRuleGroupId :: Lens.Lens' ListFirewallRuleGroupAssociations (Prelude.Maybe Prelude.Text)
listFirewallRuleGroupAssociations_firewallRuleGroupId = Lens.lens (\ListFirewallRuleGroupAssociations' {firewallRuleGroupId} -> firewallRuleGroupId) (\s@ListFirewallRuleGroupAssociations' {} a -> s {firewallRuleGroupId = a} :: ListFirewallRuleGroupAssociations)

-- | The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
listFirewallRuleGroupAssociations_maxResults :: Lens.Lens' ListFirewallRuleGroupAssociations (Prelude.Maybe Prelude.Natural)
listFirewallRuleGroupAssociations_maxResults = Lens.lens (\ListFirewallRuleGroupAssociations' {maxResults} -> maxResults) (\s@ListFirewallRuleGroupAssociations' {} a -> s {maxResults = a} :: ListFirewallRuleGroupAssociations)

-- | For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
listFirewallRuleGroupAssociations_nextToken :: Lens.Lens' ListFirewallRuleGroupAssociations (Prelude.Maybe Prelude.Text)
listFirewallRuleGroupAssociations_nextToken = Lens.lens (\ListFirewallRuleGroupAssociations' {nextToken} -> nextToken) (\s@ListFirewallRuleGroupAssociations' {} a -> s {nextToken = a} :: ListFirewallRuleGroupAssociations)

-- | The setting that determines the processing order of the rule group among
-- the rule groups that are associated with a single VPC. DNS Firewall
-- filters VPC traffic starting from the rule group with the lowest numeric
-- priority setting.
listFirewallRuleGroupAssociations_priority :: Lens.Lens' ListFirewallRuleGroupAssociations (Prelude.Maybe Prelude.Int)
listFirewallRuleGroupAssociations_priority = Lens.lens (\ListFirewallRuleGroupAssociations' {priority} -> priority) (\s@ListFirewallRuleGroupAssociations' {} a -> s {priority = a} :: ListFirewallRuleGroupAssociations)

-- | The association @Status@ setting that you want DNS Firewall to filter on
-- for the list. If you don\'t specify this, then DNS Firewall returns all
-- associations, regardless of status.
listFirewallRuleGroupAssociations_status :: Lens.Lens' ListFirewallRuleGroupAssociations (Prelude.Maybe FirewallRuleGroupAssociationStatus)
listFirewallRuleGroupAssociations_status = Lens.lens (\ListFirewallRuleGroupAssociations' {status} -> status) (\s@ListFirewallRuleGroupAssociations' {} a -> s {status = a} :: ListFirewallRuleGroupAssociations)

-- | The unique identifier of the VPC that you want to retrieve the
-- associations for. Leave this blank to retrieve associations for any VPC.
listFirewallRuleGroupAssociations_vpcId :: Lens.Lens' ListFirewallRuleGroupAssociations (Prelude.Maybe Prelude.Text)
listFirewallRuleGroupAssociations_vpcId = Lens.lens (\ListFirewallRuleGroupAssociations' {vpcId} -> vpcId) (\s@ListFirewallRuleGroupAssociations' {} a -> s {vpcId = a} :: ListFirewallRuleGroupAssociations)

instance
  Core.AWSPager
    ListFirewallRuleGroupAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallRuleGroupAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallRuleGroupAssociationsResponse_firewallRuleGroupAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFirewallRuleGroupAssociations_nextToken
          Lens..~ rs
          Lens.^? listFirewallRuleGroupAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListFirewallRuleGroupAssociations
  where
  type
    AWSResponse ListFirewallRuleGroupAssociations =
      ListFirewallRuleGroupAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallRuleGroupAssociationsResponse'
            Prelude.<$> ( x Data..?> "FirewallRuleGroupAssociations"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFirewallRuleGroupAssociations
  where
  hashWithSalt
    _salt
    ListFirewallRuleGroupAssociations' {..} =
      _salt `Prelude.hashWithSalt` firewallRuleGroupId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    ListFirewallRuleGroupAssociations
  where
  rnf ListFirewallRuleGroupAssociations' {..} =
    Prelude.rnf firewallRuleGroupId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcId

instance
  Data.ToHeaders
    ListFirewallRuleGroupAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListFirewallRuleGroupAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListFirewallRuleGroupAssociations
  where
  toJSON ListFirewallRuleGroupAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirewallRuleGroupId" Data..=)
              Prelude.<$> firewallRuleGroupId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Priority" Data..=) Prelude.<$> priority,
            ("Status" Data..=) Prelude.<$> status,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )

instance
  Data.ToPath
    ListFirewallRuleGroupAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListFirewallRuleGroupAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallRuleGroupAssociationsResponse' smart constructor.
data ListFirewallRuleGroupAssociationsResponse = ListFirewallRuleGroupAssociationsResponse'
  { -- | A list of your firewall rule group associations.
    --
    -- This might be a partial list of the associations that you have defined.
    -- For information, see @MaxResults@.
    firewallRuleGroupAssociations :: Prelude.Maybe [FirewallRuleGroupAssociation],
    -- | If objects are still available for retrieval, Resolver returns this
    -- token in the response. To retrieve the next batch of objects, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallRuleGroupAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroupAssociations', 'listFirewallRuleGroupAssociationsResponse_firewallRuleGroupAssociations' - A list of your firewall rule group associations.
--
-- This might be a partial list of the associations that you have defined.
-- For information, see @MaxResults@.
--
-- 'nextToken', 'listFirewallRuleGroupAssociationsResponse_nextToken' - If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
--
-- 'httpStatus', 'listFirewallRuleGroupAssociationsResponse_httpStatus' - The response's http status code.
newListFirewallRuleGroupAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallRuleGroupAssociationsResponse
newListFirewallRuleGroupAssociationsResponse
  pHttpStatus_ =
    ListFirewallRuleGroupAssociationsResponse'
      { firewallRuleGroupAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of your firewall rule group associations.
--
-- This might be a partial list of the associations that you have defined.
-- For information, see @MaxResults@.
listFirewallRuleGroupAssociationsResponse_firewallRuleGroupAssociations :: Lens.Lens' ListFirewallRuleGroupAssociationsResponse (Prelude.Maybe [FirewallRuleGroupAssociation])
listFirewallRuleGroupAssociationsResponse_firewallRuleGroupAssociations = Lens.lens (\ListFirewallRuleGroupAssociationsResponse' {firewallRuleGroupAssociations} -> firewallRuleGroupAssociations) (\s@ListFirewallRuleGroupAssociationsResponse' {} a -> s {firewallRuleGroupAssociations = a} :: ListFirewallRuleGroupAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
listFirewallRuleGroupAssociationsResponse_nextToken :: Lens.Lens' ListFirewallRuleGroupAssociationsResponse (Prelude.Maybe Prelude.Text)
listFirewallRuleGroupAssociationsResponse_nextToken = Lens.lens (\ListFirewallRuleGroupAssociationsResponse' {nextToken} -> nextToken) (\s@ListFirewallRuleGroupAssociationsResponse' {} a -> s {nextToken = a} :: ListFirewallRuleGroupAssociationsResponse)

-- | The response's http status code.
listFirewallRuleGroupAssociationsResponse_httpStatus :: Lens.Lens' ListFirewallRuleGroupAssociationsResponse Prelude.Int
listFirewallRuleGroupAssociationsResponse_httpStatus = Lens.lens (\ListFirewallRuleGroupAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListFirewallRuleGroupAssociationsResponse' {} a -> s {httpStatus = a} :: ListFirewallRuleGroupAssociationsResponse)

instance
  Prelude.NFData
    ListFirewallRuleGroupAssociationsResponse
  where
  rnf ListFirewallRuleGroupAssociationsResponse' {..} =
    Prelude.rnf firewallRuleGroupAssociations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
