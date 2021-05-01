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
-- Module      : Network.AWS.Config.PutExternalEvaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or updates the evaluations for process checks. This API checks if
-- the rule is a process check when the name of the AWS Config rule is
-- provided.
module Network.AWS.Config.PutExternalEvaluation
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutExternalEvaluation' smart constructor.
data PutExternalEvaluation = PutExternalEvaluation'
  { -- | The name of the AWS Config rule.
    configRuleName :: Prelude.Text,
    -- | An @ExternalEvaluation@ object that provides details about compliance.
    externalEvaluation :: ExternalEvaluation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutExternalEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'putExternalEvaluation_configRuleName' - The name of the AWS Config rule.
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

-- | The name of the AWS Config rule.
putExternalEvaluation_configRuleName :: Lens.Lens' PutExternalEvaluation Prelude.Text
putExternalEvaluation_configRuleName = Lens.lens (\PutExternalEvaluation' {configRuleName} -> configRuleName) (\s@PutExternalEvaluation' {} a -> s {configRuleName = a} :: PutExternalEvaluation)

-- | An @ExternalEvaluation@ object that provides details about compliance.
putExternalEvaluation_externalEvaluation :: Lens.Lens' PutExternalEvaluation ExternalEvaluation
putExternalEvaluation_externalEvaluation = Lens.lens (\PutExternalEvaluation' {externalEvaluation} -> externalEvaluation) (\s@PutExternalEvaluation' {} a -> s {externalEvaluation = a} :: PutExternalEvaluation)

instance Prelude.AWSRequest PutExternalEvaluation where
  type
    Rs PutExternalEvaluation =
      PutExternalEvaluationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutExternalEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutExternalEvaluation

instance Prelude.NFData PutExternalEvaluation

instance Prelude.ToHeaders PutExternalEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.PutExternalEvaluation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutExternalEvaluation where
  toJSON PutExternalEvaluation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Prelude..= configRuleName),
            Prelude.Just
              ( "ExternalEvaluation"
                  Prelude..= externalEvaluation
              )
          ]
      )

instance Prelude.ToPath PutExternalEvaluation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutExternalEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutExternalEvaluationResponse' smart constructor.
data PutExternalEvaluationResponse = PutExternalEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutExternalEvaluationResponse
