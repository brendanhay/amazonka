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
-- Module      : Amazonka.Glue.DeleteDataQualityRuleset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data quality ruleset.
module Amazonka.Glue.DeleteDataQualityRuleset
  ( -- * Creating a Request
    DeleteDataQualityRuleset (..),
    newDeleteDataQualityRuleset,

    -- * Request Lenses
    deleteDataQualityRuleset_name,

    -- * Destructuring the Response
    DeleteDataQualityRulesetResponse (..),
    newDeleteDataQualityRulesetResponse,

    -- * Response Lenses
    deleteDataQualityRulesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataQualityRuleset' smart constructor.
data DeleteDataQualityRuleset = DeleteDataQualityRuleset'
  { -- | A name for the data quality ruleset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataQualityRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteDataQualityRuleset_name' - A name for the data quality ruleset.
newDeleteDataQualityRuleset ::
  -- | 'name'
  Prelude.Text ->
  DeleteDataQualityRuleset
newDeleteDataQualityRuleset pName_ =
  DeleteDataQualityRuleset' {name = pName_}

-- | A name for the data quality ruleset.
deleteDataQualityRuleset_name :: Lens.Lens' DeleteDataQualityRuleset Prelude.Text
deleteDataQualityRuleset_name = Lens.lens (\DeleteDataQualityRuleset' {name} -> name) (\s@DeleteDataQualityRuleset' {} a -> s {name = a} :: DeleteDataQualityRuleset)

instance Core.AWSRequest DeleteDataQualityRuleset where
  type
    AWSResponse DeleteDataQualityRuleset =
      DeleteDataQualityRulesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataQualityRulesetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataQualityRuleset where
  hashWithSalt _salt DeleteDataQualityRuleset' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteDataQualityRuleset where
  rnf DeleteDataQualityRuleset' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteDataQualityRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.DeleteDataQualityRuleset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataQualityRuleset where
  toJSON DeleteDataQualityRuleset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteDataQualityRuleset where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataQualityRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataQualityRulesetResponse' smart constructor.
data DeleteDataQualityRulesetResponse = DeleteDataQualityRulesetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataQualityRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataQualityRulesetResponse_httpStatus' - The response's http status code.
newDeleteDataQualityRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataQualityRulesetResponse
newDeleteDataQualityRulesetResponse pHttpStatus_ =
  DeleteDataQualityRulesetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDataQualityRulesetResponse_httpStatus :: Lens.Lens' DeleteDataQualityRulesetResponse Prelude.Int
deleteDataQualityRulesetResponse_httpStatus = Lens.lens (\DeleteDataQualityRulesetResponse' {httpStatus} -> httpStatus) (\s@DeleteDataQualityRulesetResponse' {} a -> s {httpStatus = a} :: DeleteDataQualityRulesetResponse)

instance
  Prelude.NFData
    DeleteDataQualityRulesetResponse
  where
  rnf DeleteDataQualityRulesetResponse' {..} =
    Prelude.rnf httpStatus
