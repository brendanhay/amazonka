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
-- Module      : Amazonka.Config.PutExternalEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or updates the evaluations for process checks. This API checks if
-- the rule is a process check when the name of the Config rule is
-- provided.
module Amazonka.Config.PutExternalEvaluation
  ( -- * Creating a Request
    PutExternalEvaluation (..),
    newPutExternalEvaluation,

    -- * Request Lenses
    putExternalEvaluation_configRuleName,
    putExternalEvaluation_externalEvaluation,

    -- * Destructuring the Response
    PutExternalEvaluationResponse (..),
    newPutExternalEvaluationResponse,

    -- * Response Lenses
    putExternalEvaluationResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutExternalEvaluation' smart constructor.
data PutExternalEvaluation = PutExternalEvaluation'
  { -- | The name of the Config rule.
    configRuleName :: Prelude.Text,
    -- | An @ExternalEvaluation@ object that provides details about compliance.
    externalEvaluation :: ExternalEvaluation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutExternalEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'putExternalEvaluation_configRuleName' - The name of the Config rule.
--
-- 'externalEvaluation', 'putExternalEvaluation_externalEvaluation' - An @ExternalEvaluation@ object that provides details about compliance.
newPutExternalEvaluation ::
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'externalEvaluation'
  ExternalEvaluation ->
  PutExternalEvaluation
newPutExternalEvaluation
  pConfigRuleName_
  pExternalEvaluation_ =
    PutExternalEvaluation'
      { configRuleName =
          pConfigRuleName_,
        externalEvaluation = pExternalEvaluation_
      }

-- | The name of the Config rule.
putExternalEvaluation_configRuleName :: Lens.Lens' PutExternalEvaluation Prelude.Text
putExternalEvaluation_configRuleName = Lens.lens (\PutExternalEvaluation' {configRuleName} -> configRuleName) (\s@PutExternalEvaluation' {} a -> s {configRuleName = a} :: PutExternalEvaluation)

-- | An @ExternalEvaluation@ object that provides details about compliance.
putExternalEvaluation_externalEvaluation :: Lens.Lens' PutExternalEvaluation ExternalEvaluation
putExternalEvaluation_externalEvaluation = Lens.lens (\PutExternalEvaluation' {externalEvaluation} -> externalEvaluation) (\s@PutExternalEvaluation' {} a -> s {externalEvaluation = a} :: PutExternalEvaluation)

instance Core.AWSRequest PutExternalEvaluation where
  type
    AWSResponse PutExternalEvaluation =
      PutExternalEvaluationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutExternalEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutExternalEvaluation where
  hashWithSalt _salt PutExternalEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` externalEvaluation

instance Prelude.NFData PutExternalEvaluation where
  rnf PutExternalEvaluation' {..} =
    Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf externalEvaluation

instance Data.ToHeaders PutExternalEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutExternalEvaluation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutExternalEvaluation where
  toJSON PutExternalEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Data..= configRuleName),
            Prelude.Just
              ("ExternalEvaluation" Data..= externalEvaluation)
          ]
      )

instance Data.ToPath PutExternalEvaluation where
  toPath = Prelude.const "/"

instance Data.ToQuery PutExternalEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutExternalEvaluationResponse' smart constructor.
data PutExternalEvaluationResponse = PutExternalEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutExternalEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putExternalEvaluationResponse_httpStatus' - The response's http status code.
newPutExternalEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutExternalEvaluationResponse
newPutExternalEvaluationResponse pHttpStatus_ =
  PutExternalEvaluationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putExternalEvaluationResponse_httpStatus :: Lens.Lens' PutExternalEvaluationResponse Prelude.Int
putExternalEvaluationResponse_httpStatus = Lens.lens (\PutExternalEvaluationResponse' {httpStatus} -> httpStatus) (\s@PutExternalEvaluationResponse' {} a -> s {httpStatus = a} :: PutExternalEvaluationResponse)

instance Prelude.NFData PutExternalEvaluationResponse where
  rnf PutExternalEvaluationResponse' {..} =
    Prelude.rnf httpStatus
