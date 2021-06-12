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
-- Module      : Network.AWS.GameLift.CreateMatchmakingRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new rule set for FlexMatch matchmaking. A rule set describes
-- the type of match to create, such as the number and size of teams. It
-- also sets the parameters for acceptable player matches, such as minimum
-- skill level or character type. A rule set is used by a
-- MatchmakingConfiguration.
--
-- To create a matchmaking rule set, provide unique rule set name and the
-- rule set body in JSON format. Rule sets must be defined in the same
-- Region as the matchmaking configuration they are used with.
--
-- Since matchmaking rule sets cannot be edited, it is a good idea to check
-- the rule set syntax using ValidateMatchmakingRuleSet before creating a
-- new rule set.
--
-- __Learn more__
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a Matchmaker>
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-intro.html Matchmaking with FlexMatch>
--
-- __Related operations__
--
-- -   CreateMatchmakingConfiguration
--
-- -   DescribeMatchmakingConfigurations
--
-- -   UpdateMatchmakingConfiguration
--
-- -   DeleteMatchmakingConfiguration
--
-- -   CreateMatchmakingRuleSet
--
-- -   DescribeMatchmakingRuleSets
--
-- -   ValidateMatchmakingRuleSet
--
-- -   DeleteMatchmakingRuleSet
module Network.AWS.GameLift.CreateMatchmakingRuleSet
  ( -- * Creating a Request
    CreateMatchmakingRuleSet (..),
    newCreateMatchmakingRuleSet,

    -- * Request Lenses
    createMatchmakingRuleSet_tags,
    createMatchmakingRuleSet_name,
    createMatchmakingRuleSet_ruleSetBody,

    -- * Destructuring the Response
    CreateMatchmakingRuleSetResponse (..),
    newCreateMatchmakingRuleSetResponse,

    -- * Response Lenses
    createMatchmakingRuleSetResponse_httpStatus,
    createMatchmakingRuleSetResponse_ruleSet,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateMatchmakingRuleSet' smart constructor.
data CreateMatchmakingRuleSet = CreateMatchmakingRuleSet'
  { -- | A list of labels to assign to the new matchmaking rule set resource.
    -- Tags are developer-defined key-value pairs. Tagging AWS resources are
    -- useful for resource management, access management and cost allocation.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Tag],
    -- | A unique identifier for a matchmaking rule set. A matchmaking
    -- configuration identifies the rule set it uses by this name value. Note
    -- that the rule set name is different from the optional @name@ field in
    -- the rule set body.
    name :: Core.Text,
    -- | A collection of matchmaking rules, formatted as a JSON string. Comments
    -- are not allowed in JSON, but most elements support a description field.
    ruleSetBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMatchmakingRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMatchmakingRuleSet_tags' - A list of labels to assign to the new matchmaking rule set resource.
-- Tags are developer-defined key-value pairs. Tagging AWS resources are
-- useful for resource management, access management and cost allocation.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
--
-- 'name', 'createMatchmakingRuleSet_name' - A unique identifier for a matchmaking rule set. A matchmaking
-- configuration identifies the rule set it uses by this name value. Note
-- that the rule set name is different from the optional @name@ field in
-- the rule set body.
--
-- 'ruleSetBody', 'createMatchmakingRuleSet_ruleSetBody' - A collection of matchmaking rules, formatted as a JSON string. Comments
-- are not allowed in JSON, but most elements support a description field.
newCreateMatchmakingRuleSet ::
  -- | 'name'
  Core.Text ->
  -- | 'ruleSetBody'
  Core.Text ->
  CreateMatchmakingRuleSet
newCreateMatchmakingRuleSet pName_ pRuleSetBody_ =
  CreateMatchmakingRuleSet'
    { tags = Core.Nothing,
      name = pName_,
      ruleSetBody = pRuleSetBody_
    }

-- | A list of labels to assign to the new matchmaking rule set resource.
-- Tags are developer-defined key-value pairs. Tagging AWS resources are
-- useful for resource management, access management and cost allocation.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createMatchmakingRuleSet_tags :: Lens.Lens' CreateMatchmakingRuleSet (Core.Maybe [Tag])
createMatchmakingRuleSet_tags = Lens.lens (\CreateMatchmakingRuleSet' {tags} -> tags) (\s@CreateMatchmakingRuleSet' {} a -> s {tags = a} :: CreateMatchmakingRuleSet) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for a matchmaking rule set. A matchmaking
-- configuration identifies the rule set it uses by this name value. Note
-- that the rule set name is different from the optional @name@ field in
-- the rule set body.
createMatchmakingRuleSet_name :: Lens.Lens' CreateMatchmakingRuleSet Core.Text
createMatchmakingRuleSet_name = Lens.lens (\CreateMatchmakingRuleSet' {name} -> name) (\s@CreateMatchmakingRuleSet' {} a -> s {name = a} :: CreateMatchmakingRuleSet)

-- | A collection of matchmaking rules, formatted as a JSON string. Comments
-- are not allowed in JSON, but most elements support a description field.
createMatchmakingRuleSet_ruleSetBody :: Lens.Lens' CreateMatchmakingRuleSet Core.Text
createMatchmakingRuleSet_ruleSetBody = Lens.lens (\CreateMatchmakingRuleSet' {ruleSetBody} -> ruleSetBody) (\s@CreateMatchmakingRuleSet' {} a -> s {ruleSetBody = a} :: CreateMatchmakingRuleSet)

instance Core.AWSRequest CreateMatchmakingRuleSet where
  type
    AWSResponse CreateMatchmakingRuleSet =
      CreateMatchmakingRuleSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMatchmakingRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "RuleSet")
      )

instance Core.Hashable CreateMatchmakingRuleSet

instance Core.NFData CreateMatchmakingRuleSet

instance Core.ToHeaders CreateMatchmakingRuleSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreateMatchmakingRuleSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateMatchmakingRuleSet where
  toJSON CreateMatchmakingRuleSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just ("Name" Core..= name),
            Core.Just ("RuleSetBody" Core..= ruleSetBody)
          ]
      )

instance Core.ToPath CreateMatchmakingRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery CreateMatchmakingRuleSet where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateMatchmakingRuleSetResponse' smart constructor.
data CreateMatchmakingRuleSetResponse = CreateMatchmakingRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The newly created matchmaking rule set.
    ruleSet :: MatchmakingRuleSet
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMatchmakingRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMatchmakingRuleSetResponse_httpStatus' - The response's http status code.
--
-- 'ruleSet', 'createMatchmakingRuleSetResponse_ruleSet' - The newly created matchmaking rule set.
newCreateMatchmakingRuleSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'ruleSet'
  MatchmakingRuleSet ->
  CreateMatchmakingRuleSetResponse
newCreateMatchmakingRuleSetResponse
  pHttpStatus_
  pRuleSet_ =
    CreateMatchmakingRuleSetResponse'
      { httpStatus =
          pHttpStatus_,
        ruleSet = pRuleSet_
      }

-- | The response's http status code.
createMatchmakingRuleSetResponse_httpStatus :: Lens.Lens' CreateMatchmakingRuleSetResponse Core.Int
createMatchmakingRuleSetResponse_httpStatus = Lens.lens (\CreateMatchmakingRuleSetResponse' {httpStatus} -> httpStatus) (\s@CreateMatchmakingRuleSetResponse' {} a -> s {httpStatus = a} :: CreateMatchmakingRuleSetResponse)

-- | The newly created matchmaking rule set.
createMatchmakingRuleSetResponse_ruleSet :: Lens.Lens' CreateMatchmakingRuleSetResponse MatchmakingRuleSet
createMatchmakingRuleSetResponse_ruleSet = Lens.lens (\CreateMatchmakingRuleSetResponse' {ruleSet} -> ruleSet) (\s@CreateMatchmakingRuleSetResponse' {} a -> s {ruleSet = a} :: CreateMatchmakingRuleSetResponse)

instance Core.NFData CreateMatchmakingRuleSetResponse
