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
-- Module      : Network.AWS.GameLift.ValidateMatchmakingRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the syntax of a matchmaking rule or rule set. This operation
-- checks that the rule set is using syntactically correct JSON and that it
-- conforms to allowed property expressions. To validate syntax, provide a
-- rule set JSON string.
--
-- __Learn more__
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
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
module Network.AWS.GameLift.ValidateMatchmakingRuleSet
  ( -- * Creating a Request
    ValidateMatchmakingRuleSet (..),
    newValidateMatchmakingRuleSet,

    -- * Request Lenses
    validateMatchmakingRuleSet_ruleSetBody,

    -- * Destructuring the Response
    ValidateMatchmakingRuleSetResponse (..),
    newValidateMatchmakingRuleSetResponse,

    -- * Response Lenses
    validateMatchmakingRuleSetResponse_valid,
    validateMatchmakingRuleSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newValidateMatchmakingRuleSet' smart constructor.
data ValidateMatchmakingRuleSet = ValidateMatchmakingRuleSet'
  { -- | A collection of matchmaking rules to validate, formatted as a JSON
    -- string.
    ruleSetBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidateMatchmakingRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleSetBody', 'validateMatchmakingRuleSet_ruleSetBody' - A collection of matchmaking rules to validate, formatted as a JSON
-- string.
newValidateMatchmakingRuleSet ::
  -- | 'ruleSetBody'
  Core.Text ->
  ValidateMatchmakingRuleSet
newValidateMatchmakingRuleSet pRuleSetBody_ =
  ValidateMatchmakingRuleSet'
    { ruleSetBody =
        pRuleSetBody_
    }

-- | A collection of matchmaking rules to validate, formatted as a JSON
-- string.
validateMatchmakingRuleSet_ruleSetBody :: Lens.Lens' ValidateMatchmakingRuleSet Core.Text
validateMatchmakingRuleSet_ruleSetBody = Lens.lens (\ValidateMatchmakingRuleSet' {ruleSetBody} -> ruleSetBody) (\s@ValidateMatchmakingRuleSet' {} a -> s {ruleSetBody = a} :: ValidateMatchmakingRuleSet)

instance Core.AWSRequest ValidateMatchmakingRuleSet where
  type
    AWSResponse ValidateMatchmakingRuleSet =
      ValidateMatchmakingRuleSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateMatchmakingRuleSetResponse'
            Core.<$> (x Core..?> "Valid")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ValidateMatchmakingRuleSet

instance Core.NFData ValidateMatchmakingRuleSet

instance Core.ToHeaders ValidateMatchmakingRuleSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.ValidateMatchmakingRuleSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ValidateMatchmakingRuleSet where
  toJSON ValidateMatchmakingRuleSet' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RuleSetBody" Core..= ruleSetBody)]
      )

instance Core.ToPath ValidateMatchmakingRuleSet where
  toPath = Core.const "/"

instance Core.ToQuery ValidateMatchmakingRuleSet where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newValidateMatchmakingRuleSetResponse' smart constructor.
data ValidateMatchmakingRuleSetResponse = ValidateMatchmakingRuleSetResponse'
  { -- | A response indicating whether the rule set is valid.
    valid :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidateMatchmakingRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valid', 'validateMatchmakingRuleSetResponse_valid' - A response indicating whether the rule set is valid.
--
-- 'httpStatus', 'validateMatchmakingRuleSetResponse_httpStatus' - The response's http status code.
newValidateMatchmakingRuleSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ValidateMatchmakingRuleSetResponse
newValidateMatchmakingRuleSetResponse pHttpStatus_ =
  ValidateMatchmakingRuleSetResponse'
    { valid =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A response indicating whether the rule set is valid.
validateMatchmakingRuleSetResponse_valid :: Lens.Lens' ValidateMatchmakingRuleSetResponse (Core.Maybe Core.Bool)
validateMatchmakingRuleSetResponse_valid = Lens.lens (\ValidateMatchmakingRuleSetResponse' {valid} -> valid) (\s@ValidateMatchmakingRuleSetResponse' {} a -> s {valid = a} :: ValidateMatchmakingRuleSetResponse)

-- | The response's http status code.
validateMatchmakingRuleSetResponse_httpStatus :: Lens.Lens' ValidateMatchmakingRuleSetResponse Core.Int
validateMatchmakingRuleSetResponse_httpStatus = Lens.lens (\ValidateMatchmakingRuleSetResponse' {httpStatus} -> httpStatus) (\s@ValidateMatchmakingRuleSetResponse' {} a -> s {httpStatus = a} :: ValidateMatchmakingRuleSetResponse)

instance
  Core.NFData
    ValidateMatchmakingRuleSetResponse
