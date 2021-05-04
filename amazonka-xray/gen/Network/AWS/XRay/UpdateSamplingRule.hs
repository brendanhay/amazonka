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
-- Module      : Network.AWS.XRay.UpdateSamplingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a sampling rule\'s configuration.
module Network.AWS.XRay.UpdateSamplingRule
  ( -- * Creating a Request
    UpdateSamplingRule (..),
    newUpdateSamplingRule,

    -- * Request Lenses
    updateSamplingRule_samplingRuleUpdate,

    -- * Destructuring the Response
    UpdateSamplingRuleResponse (..),
    newUpdateSamplingRuleResponse,

    -- * Response Lenses
    updateSamplingRuleResponse_samplingRuleRecord,
    updateSamplingRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newUpdateSamplingRule' smart constructor.
data UpdateSamplingRule = UpdateSamplingRule'
  { -- | The rule and fields to change.
    samplingRuleUpdate :: SamplingRuleUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSamplingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRuleUpdate', 'updateSamplingRule_samplingRuleUpdate' - The rule and fields to change.
newUpdateSamplingRule ::
  -- | 'samplingRuleUpdate'
  SamplingRuleUpdate ->
  UpdateSamplingRule
newUpdateSamplingRule pSamplingRuleUpdate_ =
  UpdateSamplingRule'
    { samplingRuleUpdate =
        pSamplingRuleUpdate_
    }

-- | The rule and fields to change.
updateSamplingRule_samplingRuleUpdate :: Lens.Lens' UpdateSamplingRule SamplingRuleUpdate
updateSamplingRule_samplingRuleUpdate = Lens.lens (\UpdateSamplingRule' {samplingRuleUpdate} -> samplingRuleUpdate) (\s@UpdateSamplingRule' {} a -> s {samplingRuleUpdate = a} :: UpdateSamplingRule)

instance Prelude.AWSRequest UpdateSamplingRule where
  type
    Rs UpdateSamplingRule =
      UpdateSamplingRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSamplingRuleResponse'
            Prelude.<$> (x Prelude..?> "SamplingRuleRecord")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSamplingRule

instance Prelude.NFData UpdateSamplingRule

instance Prelude.ToHeaders UpdateSamplingRule where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateSamplingRule where
  toJSON UpdateSamplingRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SamplingRuleUpdate"
                  Prelude..= samplingRuleUpdate
              )
          ]
      )

instance Prelude.ToPath UpdateSamplingRule where
  toPath = Prelude.const "/UpdateSamplingRule"

instance Prelude.ToQuery UpdateSamplingRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSamplingRuleResponse' smart constructor.
data UpdateSamplingRuleResponse = UpdateSamplingRuleResponse'
  { -- | The updated rule definition and metadata.
    samplingRuleRecord :: Prelude.Maybe SamplingRuleRecord,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSamplingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRuleRecord', 'updateSamplingRuleResponse_samplingRuleRecord' - The updated rule definition and metadata.
--
-- 'httpStatus', 'updateSamplingRuleResponse_httpStatus' - The response's http status code.
newUpdateSamplingRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSamplingRuleResponse
newUpdateSamplingRuleResponse pHttpStatus_ =
  UpdateSamplingRuleResponse'
    { samplingRuleRecord =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated rule definition and metadata.
updateSamplingRuleResponse_samplingRuleRecord :: Lens.Lens' UpdateSamplingRuleResponse (Prelude.Maybe SamplingRuleRecord)
updateSamplingRuleResponse_samplingRuleRecord = Lens.lens (\UpdateSamplingRuleResponse' {samplingRuleRecord} -> samplingRuleRecord) (\s@UpdateSamplingRuleResponse' {} a -> s {samplingRuleRecord = a} :: UpdateSamplingRuleResponse)

-- | The response's http status code.
updateSamplingRuleResponse_httpStatus :: Lens.Lens' UpdateSamplingRuleResponse Prelude.Int
updateSamplingRuleResponse_httpStatus = Lens.lens (\UpdateSamplingRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateSamplingRuleResponse' {} a -> s {httpStatus = a} :: UpdateSamplingRuleResponse)

instance Prelude.NFData UpdateSamplingRuleResponse
