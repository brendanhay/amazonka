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
-- Module      : Amazonka.Route53Resolver.CreateFirewallRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty DNS Firewall rule group for filtering DNS network
-- traffic in a VPC. You can add rules to the new rule group by calling
-- CreateFirewallRule.
module Amazonka.Route53Resolver.CreateFirewallRuleGroup
  ( -- * Creating a Request
    CreateFirewallRuleGroup (..),
    newCreateFirewallRuleGroup,

    -- * Request Lenses
    createFirewallRuleGroup_tags,
    createFirewallRuleGroup_creatorRequestId,
    createFirewallRuleGroup_name,

    -- * Destructuring the Response
    CreateFirewallRuleGroupResponse (..),
    newCreateFirewallRuleGroupResponse,

    -- * Response Lenses
    createFirewallRuleGroupResponse_firewallRuleGroup,
    createFirewallRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newCreateFirewallRuleGroup' smart constructor.
data CreateFirewallRuleGroup = CreateFirewallRuleGroup'
  { -- | A list of the tag keys and values that you want to associate with the
    -- rule group.
    tags :: Prelude.Maybe [Tag],
    -- | A unique string defined by you to identify the request. This allows you
    -- to retry failed requests without the risk of running the operation
    -- twice. This can be any unique string, for example, a timestamp.
    creatorRequestId :: Prelude.Text,
    -- | A name that lets you identify the rule group, to manage and use it.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewallRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFirewallRuleGroup_tags' - A list of the tag keys and values that you want to associate with the
-- rule group.
--
-- 'creatorRequestId', 'createFirewallRuleGroup_creatorRequestId' - A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
--
-- 'name', 'createFirewallRuleGroup_name' - A name that lets you identify the rule group, to manage and use it.
newCreateFirewallRuleGroup ::
  -- | 'creatorRequestId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateFirewallRuleGroup
newCreateFirewallRuleGroup pCreatorRequestId_ pName_ =
  CreateFirewallRuleGroup'
    { tags = Prelude.Nothing,
      creatorRequestId = pCreatorRequestId_,
      name = pName_
    }

-- | A list of the tag keys and values that you want to associate with the
-- rule group.
createFirewallRuleGroup_tags :: Lens.Lens' CreateFirewallRuleGroup (Prelude.Maybe [Tag])
createFirewallRuleGroup_tags = Lens.lens (\CreateFirewallRuleGroup' {tags} -> tags) (\s@CreateFirewallRuleGroup' {} a -> s {tags = a} :: CreateFirewallRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
createFirewallRuleGroup_creatorRequestId :: Lens.Lens' CreateFirewallRuleGroup Prelude.Text
createFirewallRuleGroup_creatorRequestId = Lens.lens (\CreateFirewallRuleGroup' {creatorRequestId} -> creatorRequestId) (\s@CreateFirewallRuleGroup' {} a -> s {creatorRequestId = a} :: CreateFirewallRuleGroup)

-- | A name that lets you identify the rule group, to manage and use it.
createFirewallRuleGroup_name :: Lens.Lens' CreateFirewallRuleGroup Prelude.Text
createFirewallRuleGroup_name = Lens.lens (\CreateFirewallRuleGroup' {name} -> name) (\s@CreateFirewallRuleGroup' {} a -> s {name = a} :: CreateFirewallRuleGroup)

instance Core.AWSRequest CreateFirewallRuleGroup where
  type
    AWSResponse CreateFirewallRuleGroup =
      CreateFirewallRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFirewallRuleGroupResponse'
            Prelude.<$> (x Data..?> "FirewallRuleGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFirewallRuleGroup where
  hashWithSalt _salt CreateFirewallRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFirewallRuleGroup where
  rnf CreateFirewallRuleGroup' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf creatorRequestId `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders CreateFirewallRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.CreateFirewallRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFirewallRuleGroup where
  toJSON CreateFirewallRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("CreatorRequestId" Data..= creatorRequestId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateFirewallRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFirewallRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFirewallRuleGroupResponse' smart constructor.
data CreateFirewallRuleGroupResponse = CreateFirewallRuleGroupResponse'
  { -- | A collection of rules used to filter DNS network traffic.
    firewallRuleGroup :: Prelude.Maybe FirewallRuleGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewallRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroup', 'createFirewallRuleGroupResponse_firewallRuleGroup' - A collection of rules used to filter DNS network traffic.
--
-- 'httpStatus', 'createFirewallRuleGroupResponse_httpStatus' - The response's http status code.
newCreateFirewallRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFirewallRuleGroupResponse
newCreateFirewallRuleGroupResponse pHttpStatus_ =
  CreateFirewallRuleGroupResponse'
    { firewallRuleGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of rules used to filter DNS network traffic.
createFirewallRuleGroupResponse_firewallRuleGroup :: Lens.Lens' CreateFirewallRuleGroupResponse (Prelude.Maybe FirewallRuleGroup)
createFirewallRuleGroupResponse_firewallRuleGroup = Lens.lens (\CreateFirewallRuleGroupResponse' {firewallRuleGroup} -> firewallRuleGroup) (\s@CreateFirewallRuleGroupResponse' {} a -> s {firewallRuleGroup = a} :: CreateFirewallRuleGroupResponse)

-- | The response's http status code.
createFirewallRuleGroupResponse_httpStatus :: Lens.Lens' CreateFirewallRuleGroupResponse Prelude.Int
createFirewallRuleGroupResponse_httpStatus = Lens.lens (\CreateFirewallRuleGroupResponse' {httpStatus} -> httpStatus) (\s@CreateFirewallRuleGroupResponse' {} a -> s {httpStatus = a} :: CreateFirewallRuleGroupResponse)

instance
  Prelude.NFData
    CreateFirewallRuleGroupResponse
  where
  rnf CreateFirewallRuleGroupResponse' {..} =
    Prelude.rnf firewallRuleGroup `Prelude.seq`
      Prelude.rnf httpStatus
