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
-- Module      : Network.AWS.Config.DeleteEvaluationResults
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the evaluation results for the specified AWS Config rule. You
-- can specify one AWS Config rule per request. After you delete the
-- evaluation results, you can call the StartConfigRulesEvaluation API to
-- start evaluating your AWS resources against the rule.
module Network.AWS.Config.DeleteEvaluationResults
  ( -- * Creating a Request
    DeleteEvaluationResults (..),
    newDeleteEvaluationResults,

    -- * Request Lenses
    deleteEvaluationResults_configRuleName,

    -- * Destructuring the Response
    DeleteEvaluationResultsResponse (..),
    newDeleteEvaluationResultsResponse,

    -- * Response Lenses
    deleteEvaluationResultsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteEvaluationResults' smart constructor.
data DeleteEvaluationResults = DeleteEvaluationResults'
  { -- | The name of the AWS Config rule for which you want to delete the
    -- evaluation results.
    configRuleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEvaluationResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'deleteEvaluationResults_configRuleName' - The name of the AWS Config rule for which you want to delete the
-- evaluation results.
newDeleteEvaluationResults ::
  -- | 'configRuleName'
  Core.Text ->
  DeleteEvaluationResults
newDeleteEvaluationResults pConfigRuleName_ =
  DeleteEvaluationResults'
    { configRuleName =
        pConfigRuleName_
    }

-- | The name of the AWS Config rule for which you want to delete the
-- evaluation results.
deleteEvaluationResults_configRuleName :: Lens.Lens' DeleteEvaluationResults Core.Text
deleteEvaluationResults_configRuleName = Lens.lens (\DeleteEvaluationResults' {configRuleName} -> configRuleName) (\s@DeleteEvaluationResults' {} a -> s {configRuleName = a} :: DeleteEvaluationResults)

instance Core.AWSRequest DeleteEvaluationResults where
  type
    AWSResponse DeleteEvaluationResults =
      DeleteEvaluationResultsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEvaluationResultsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteEvaluationResults

instance Core.NFData DeleteEvaluationResults

instance Core.ToHeaders DeleteEvaluationResults where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteEvaluationResults" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteEvaluationResults where
  toJSON DeleteEvaluationResults' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConfigRuleName" Core..= configRuleName)
          ]
      )

instance Core.ToPath DeleteEvaluationResults where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEvaluationResults where
  toQuery = Core.const Core.mempty

-- | The output when you delete the evaluation results for the specified AWS
-- Config rule.
--
-- /See:/ 'newDeleteEvaluationResultsResponse' smart constructor.
data DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEvaluationResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEvaluationResultsResponse_httpStatus' - The response's http status code.
newDeleteEvaluationResultsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteEvaluationResultsResponse
newDeleteEvaluationResultsResponse pHttpStatus_ =
  DeleteEvaluationResultsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEvaluationResultsResponse_httpStatus :: Lens.Lens' DeleteEvaluationResultsResponse Core.Int
deleteEvaluationResultsResponse_httpStatus = Lens.lens (\DeleteEvaluationResultsResponse' {httpStatus} -> httpStatus) (\s@DeleteEvaluationResultsResponse' {} a -> s {httpStatus = a} :: DeleteEvaluationResultsResponse)

instance Core.NFData DeleteEvaluationResultsResponse
