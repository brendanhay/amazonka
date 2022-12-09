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
-- Module      : Amazonka.GameLift.DeleteMatchmakingRuleSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing matchmaking rule set. To delete the rule set,
-- provide the rule set name. Rule sets cannot be deleted if they are
-- currently being used by a matchmaking configuration.
--
-- __Learn more__
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a rule set>
module Amazonka.GameLift.DeleteMatchmakingRuleSet
  ( -- * Creating a Request
    DeleteMatchmakingRuleSet (..),
    newDeleteMatchmakingRuleSet,

    -- * Request Lenses
    deleteMatchmakingRuleSet_name,

    -- * Destructuring the Response
    DeleteMatchmakingRuleSetResponse (..),
    newDeleteMatchmakingRuleSetResponse,

    -- * Response Lenses
    deleteMatchmakingRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMatchmakingRuleSet' smart constructor.
data DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSet'
  { -- | A unique identifier for the matchmaking rule set to be deleted. (Note:
    -- The rule set name is different from the optional \"name\" field in the
    -- rule set body.) You can use either the rule set name or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMatchmakingRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteMatchmakingRuleSet_name' - A unique identifier for the matchmaking rule set to be deleted. (Note:
-- The rule set name is different from the optional \"name\" field in the
-- rule set body.) You can use either the rule set name or ARN value.
newDeleteMatchmakingRuleSet ::
  -- | 'name'
  Prelude.Text ->
  DeleteMatchmakingRuleSet
newDeleteMatchmakingRuleSet pName_ =
  DeleteMatchmakingRuleSet' {name = pName_}

-- | A unique identifier for the matchmaking rule set to be deleted. (Note:
-- The rule set name is different from the optional \"name\" field in the
-- rule set body.) You can use either the rule set name or ARN value.
deleteMatchmakingRuleSet_name :: Lens.Lens' DeleteMatchmakingRuleSet Prelude.Text
deleteMatchmakingRuleSet_name = Lens.lens (\DeleteMatchmakingRuleSet' {name} -> name) (\s@DeleteMatchmakingRuleSet' {} a -> s {name = a} :: DeleteMatchmakingRuleSet)

instance Core.AWSRequest DeleteMatchmakingRuleSet where
  type
    AWSResponse DeleteMatchmakingRuleSet =
      DeleteMatchmakingRuleSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMatchmakingRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMatchmakingRuleSet where
  hashWithSalt _salt DeleteMatchmakingRuleSet' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteMatchmakingRuleSet where
  rnf DeleteMatchmakingRuleSet' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteMatchmakingRuleSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DeleteMatchmakingRuleSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMatchmakingRuleSet where
  toJSON DeleteMatchmakingRuleSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteMatchmakingRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMatchmakingRuleSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMatchmakingRuleSetResponse' smart constructor.
data DeleteMatchmakingRuleSetResponse = DeleteMatchmakingRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMatchmakingRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMatchmakingRuleSetResponse_httpStatus' - The response's http status code.
newDeleteMatchmakingRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMatchmakingRuleSetResponse
newDeleteMatchmakingRuleSetResponse pHttpStatus_ =
  DeleteMatchmakingRuleSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMatchmakingRuleSetResponse_httpStatus :: Lens.Lens' DeleteMatchmakingRuleSetResponse Prelude.Int
deleteMatchmakingRuleSetResponse_httpStatus = Lens.lens (\DeleteMatchmakingRuleSetResponse' {httpStatus} -> httpStatus) (\s@DeleteMatchmakingRuleSetResponse' {} a -> s {httpStatus = a} :: DeleteMatchmakingRuleSetResponse)

instance
  Prelude.NFData
    DeleteMatchmakingRuleSetResponse
  where
  rnf DeleteMatchmakingRuleSetResponse' {..} =
    Prelude.rnf httpStatus
