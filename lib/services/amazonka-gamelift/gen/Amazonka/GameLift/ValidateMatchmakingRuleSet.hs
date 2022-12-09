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
-- Module      : Amazonka.GameLift.ValidateMatchmakingRuleSet
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a rule set>
module Amazonka.GameLift.ValidateMatchmakingRuleSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidateMatchmakingRuleSet' smart constructor.
data ValidateMatchmakingRuleSet = ValidateMatchmakingRuleSet'
  { -- | A collection of matchmaking rules to validate, formatted as a JSON
    -- string.
    ruleSetBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ValidateMatchmakingRuleSet
newValidateMatchmakingRuleSet pRuleSetBody_ =
  ValidateMatchmakingRuleSet'
    { ruleSetBody =
        pRuleSetBody_
    }

-- | A collection of matchmaking rules to validate, formatted as a JSON
-- string.
validateMatchmakingRuleSet_ruleSetBody :: Lens.Lens' ValidateMatchmakingRuleSet Prelude.Text
validateMatchmakingRuleSet_ruleSetBody = Lens.lens (\ValidateMatchmakingRuleSet' {ruleSetBody} -> ruleSetBody) (\s@ValidateMatchmakingRuleSet' {} a -> s {ruleSetBody = a} :: ValidateMatchmakingRuleSet)

instance Core.AWSRequest ValidateMatchmakingRuleSet where
  type
    AWSResponse ValidateMatchmakingRuleSet =
      ValidateMatchmakingRuleSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateMatchmakingRuleSetResponse'
            Prelude.<$> (x Data..?> "Valid")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ValidateMatchmakingRuleSet where
  hashWithSalt _salt ValidateMatchmakingRuleSet' {..} =
    _salt `Prelude.hashWithSalt` ruleSetBody

instance Prelude.NFData ValidateMatchmakingRuleSet where
  rnf ValidateMatchmakingRuleSet' {..} =
    Prelude.rnf ruleSetBody

instance Data.ToHeaders ValidateMatchmakingRuleSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.ValidateMatchmakingRuleSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ValidateMatchmakingRuleSet where
  toJSON ValidateMatchmakingRuleSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RuleSetBody" Data..= ruleSetBody)]
      )

instance Data.ToPath ValidateMatchmakingRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery ValidateMatchmakingRuleSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateMatchmakingRuleSetResponse' smart constructor.
data ValidateMatchmakingRuleSetResponse = ValidateMatchmakingRuleSetResponse'
  { -- | A response indicating whether the rule set is valid.
    valid :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ValidateMatchmakingRuleSetResponse
newValidateMatchmakingRuleSetResponse pHttpStatus_ =
  ValidateMatchmakingRuleSetResponse'
    { valid =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A response indicating whether the rule set is valid.
validateMatchmakingRuleSetResponse_valid :: Lens.Lens' ValidateMatchmakingRuleSetResponse (Prelude.Maybe Prelude.Bool)
validateMatchmakingRuleSetResponse_valid = Lens.lens (\ValidateMatchmakingRuleSetResponse' {valid} -> valid) (\s@ValidateMatchmakingRuleSetResponse' {} a -> s {valid = a} :: ValidateMatchmakingRuleSetResponse)

-- | The response's http status code.
validateMatchmakingRuleSetResponse_httpStatus :: Lens.Lens' ValidateMatchmakingRuleSetResponse Prelude.Int
validateMatchmakingRuleSetResponse_httpStatus = Lens.lens (\ValidateMatchmakingRuleSetResponse' {httpStatus} -> httpStatus) (\s@ValidateMatchmakingRuleSetResponse' {} a -> s {httpStatus = a} :: ValidateMatchmakingRuleSetResponse)

instance
  Prelude.NFData
    ValidateMatchmakingRuleSetResponse
  where
  rnf ValidateMatchmakingRuleSetResponse' {..} =
    Prelude.rnf valid
      `Prelude.seq` Prelude.rnf httpStatus
