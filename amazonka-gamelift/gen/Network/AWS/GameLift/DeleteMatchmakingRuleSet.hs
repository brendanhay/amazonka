{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.DeleteMatchmakingRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.GameLift.DeleteMatchmakingRuleSet
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteMatchmakingRuleSet' smart constructor.
data DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSet'
  { -- | A unique identifier for a matchmaking rule set to be deleted. (Note: The
    -- rule set name is different from the optional \"name\" field in the rule
    -- set body.) You can use either the rule set name or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMatchmakingRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteMatchmakingRuleSet_name' - A unique identifier for a matchmaking rule set to be deleted. (Note: The
-- rule set name is different from the optional \"name\" field in the rule
-- set body.) You can use either the rule set name or ARN value.
newDeleteMatchmakingRuleSet ::
  -- | 'name'
  Prelude.Text ->
  DeleteMatchmakingRuleSet
newDeleteMatchmakingRuleSet pName_ =
  DeleteMatchmakingRuleSet' {name = pName_}

-- | A unique identifier for a matchmaking rule set to be deleted. (Note: The
-- rule set name is different from the optional \"name\" field in the rule
-- set body.) You can use either the rule set name or ARN value.
deleteMatchmakingRuleSet_name :: Lens.Lens' DeleteMatchmakingRuleSet Prelude.Text
deleteMatchmakingRuleSet_name = Lens.lens (\DeleteMatchmakingRuleSet' {name} -> name) (\s@DeleteMatchmakingRuleSet' {} a -> s {name = a} :: DeleteMatchmakingRuleSet)

instance Prelude.AWSRequest DeleteMatchmakingRuleSet where
  type
    Rs DeleteMatchmakingRuleSet =
      DeleteMatchmakingRuleSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMatchmakingRuleSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMatchmakingRuleSet

instance Prelude.NFData DeleteMatchmakingRuleSet

instance Prelude.ToHeaders DeleteMatchmakingRuleSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DeleteMatchmakingRuleSet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteMatchmakingRuleSet where
  toJSON DeleteMatchmakingRuleSet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteMatchmakingRuleSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteMatchmakingRuleSet where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDeleteMatchmakingRuleSetResponse' smart constructor.
data DeleteMatchmakingRuleSetResponse = DeleteMatchmakingRuleSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
