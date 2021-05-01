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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteEvaluationResults' smart constructor.
data DeleteEvaluationResults = DeleteEvaluationResults'
  { -- | The name of the AWS Config rule for which you want to delete the
    -- evaluation results.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteEvaluationResults
newDeleteEvaluationResults pConfigRuleName_ =
  DeleteEvaluationResults'
    { configRuleName =
        pConfigRuleName_
    }

-- | The name of the AWS Config rule for which you want to delete the
-- evaluation results.
deleteEvaluationResults_configRuleName :: Lens.Lens' DeleteEvaluationResults Prelude.Text
deleteEvaluationResults_configRuleName = Lens.lens (\DeleteEvaluationResults' {configRuleName} -> configRuleName) (\s@DeleteEvaluationResults' {} a -> s {configRuleName = a} :: DeleteEvaluationResults)

instance Prelude.AWSRequest DeleteEvaluationResults where
  type
    Rs DeleteEvaluationResults =
      DeleteEvaluationResultsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEvaluationResultsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEvaluationResults

instance Prelude.NFData DeleteEvaluationResults

instance Prelude.ToHeaders DeleteEvaluationResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteEvaluationResults" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEvaluationResults where
  toJSON DeleteEvaluationResults' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Prelude..= configRuleName)
          ]
      )

instance Prelude.ToPath DeleteEvaluationResults where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEvaluationResults where
  toQuery = Prelude.const Prelude.mempty

-- | The output when you delete the evaluation results for the specified AWS
-- Config rule.
--
-- /See:/ 'newDeleteEvaluationResultsResponse' smart constructor.
data DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteEvaluationResultsResponse
newDeleteEvaluationResultsResponse pHttpStatus_ =
  DeleteEvaluationResultsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEvaluationResultsResponse_httpStatus :: Lens.Lens' DeleteEvaluationResultsResponse Prelude.Int
deleteEvaluationResultsResponse_httpStatus = Lens.lens (\DeleteEvaluationResultsResponse' {httpStatus} -> httpStatus) (\s@DeleteEvaluationResultsResponse' {} a -> s {httpStatus = a} :: DeleteEvaluationResultsResponse)

instance
  Prelude.NFData
    DeleteEvaluationResultsResponse
