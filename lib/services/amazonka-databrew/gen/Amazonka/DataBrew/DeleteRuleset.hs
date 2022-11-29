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
-- Module      : Amazonka.DataBrew.DeleteRuleset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a ruleset.
module Amazonka.DataBrew.DeleteRuleset
  ( -- * Creating a Request
    DeleteRuleset (..),
    newDeleteRuleset,

    -- * Request Lenses
    deleteRuleset_name,

    -- * Destructuring the Response
    DeleteRulesetResponse (..),
    newDeleteRulesetResponse,

    -- * Response Lenses
    deleteRulesetResponse_httpStatus,
    deleteRulesetResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRuleset' smart constructor.
data DeleteRuleset = DeleteRuleset'
  { -- | The name of the ruleset to be deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteRuleset_name' - The name of the ruleset to be deleted.
newDeleteRuleset ::
  -- | 'name'
  Prelude.Text ->
  DeleteRuleset
newDeleteRuleset pName_ =
  DeleteRuleset' {name = pName_}

-- | The name of the ruleset to be deleted.
deleteRuleset_name :: Lens.Lens' DeleteRuleset Prelude.Text
deleteRuleset_name = Lens.lens (\DeleteRuleset' {name} -> name) (\s@DeleteRuleset' {} a -> s {name = a} :: DeleteRuleset)

instance Core.AWSRequest DeleteRuleset where
  type
    AWSResponse DeleteRuleset =
      DeleteRulesetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRulesetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
      )

instance Prelude.Hashable DeleteRuleset where
  hashWithSalt _salt DeleteRuleset' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteRuleset where
  rnf DeleteRuleset' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteRuleset where
  toPath DeleteRuleset' {..} =
    Prelude.mconcat ["/rulesets/", Core.toBS name]

instance Core.ToQuery DeleteRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRulesetResponse' smart constructor.
data DeleteRulesetResponse = DeleteRulesetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the deleted ruleset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRulesetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'deleteRulesetResponse_name' - The name of the deleted ruleset.
newDeleteRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DeleteRulesetResponse
newDeleteRulesetResponse pHttpStatus_ pName_ =
  DeleteRulesetResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
deleteRulesetResponse_httpStatus :: Lens.Lens' DeleteRulesetResponse Prelude.Int
deleteRulesetResponse_httpStatus = Lens.lens (\DeleteRulesetResponse' {httpStatus} -> httpStatus) (\s@DeleteRulesetResponse' {} a -> s {httpStatus = a} :: DeleteRulesetResponse)

-- | The name of the deleted ruleset.
deleteRulesetResponse_name :: Lens.Lens' DeleteRulesetResponse Prelude.Text
deleteRulesetResponse_name = Lens.lens (\DeleteRulesetResponse' {name} -> name) (\s@DeleteRulesetResponse' {} a -> s {name = a} :: DeleteRulesetResponse)

instance Prelude.NFData DeleteRulesetResponse where
  rnf DeleteRulesetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
