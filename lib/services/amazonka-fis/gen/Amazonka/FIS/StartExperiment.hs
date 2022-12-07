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
-- Module      : Amazonka.FIS.StartExperiment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts running an experiment from the specified experiment template.
module Amazonka.FIS.StartExperiment
  ( -- * Creating a Request
    StartExperiment (..),
    newStartExperiment,

    -- * Request Lenses
    startExperiment_tags,
    startExperiment_clientToken,
    startExperiment_experimentTemplateId,

    -- * Destructuring the Response
    StartExperimentResponse (..),
    newStartExperimentResponse,

    -- * Response Lenses
    startExperimentResponse_experiment,
    startExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartExperiment' smart constructor.
data StartExperiment = StartExperiment'
  { -- | The tags to apply to the experiment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text,
    -- | The ID of the experiment template.
    experimentTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startExperiment_tags' - The tags to apply to the experiment.
--
-- 'clientToken', 'startExperiment_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'experimentTemplateId', 'startExperiment_experimentTemplateId' - The ID of the experiment template.
newStartExperiment ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'experimentTemplateId'
  Prelude.Text ->
  StartExperiment
newStartExperiment
  pClientToken_
  pExperimentTemplateId_ =
    StartExperiment'
      { tags = Prelude.Nothing,
        clientToken = pClientToken_,
        experimentTemplateId = pExperimentTemplateId_
      }

-- | The tags to apply to the experiment.
startExperiment_tags :: Lens.Lens' StartExperiment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startExperiment_tags = Lens.lens (\StartExperiment' {tags} -> tags) (\s@StartExperiment' {} a -> s {tags = a} :: StartExperiment) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
startExperiment_clientToken :: Lens.Lens' StartExperiment Prelude.Text
startExperiment_clientToken = Lens.lens (\StartExperiment' {clientToken} -> clientToken) (\s@StartExperiment' {} a -> s {clientToken = a} :: StartExperiment)

-- | The ID of the experiment template.
startExperiment_experimentTemplateId :: Lens.Lens' StartExperiment Prelude.Text
startExperiment_experimentTemplateId = Lens.lens (\StartExperiment' {experimentTemplateId} -> experimentTemplateId) (\s@StartExperiment' {} a -> s {experimentTemplateId = a} :: StartExperiment)

instance Core.AWSRequest StartExperiment where
  type
    AWSResponse StartExperiment =
      StartExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExperimentResponse'
            Prelude.<$> (x Data..?> "experiment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExperiment where
  hashWithSalt _salt StartExperiment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` experimentTemplateId

instance Prelude.NFData StartExperiment where
  rnf StartExperiment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf experimentTemplateId

instance Data.ToHeaders StartExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartExperiment where
  toJSON StartExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("clientToken" Data..= clientToken),
            Prelude.Just
              ( "experimentTemplateId"
                  Data..= experimentTemplateId
              )
          ]
      )

instance Data.ToPath StartExperiment where
  toPath = Prelude.const "/experiments"

instance Data.ToQuery StartExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExperimentResponse' smart constructor.
data StartExperimentResponse = StartExperimentResponse'
  { -- | Information about the experiment.
    experiment :: Prelude.Maybe Experiment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiment', 'startExperimentResponse_experiment' - Information about the experiment.
--
-- 'httpStatus', 'startExperimentResponse_httpStatus' - The response's http status code.
newStartExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartExperimentResponse
newStartExperimentResponse pHttpStatus_ =
  StartExperimentResponse'
    { experiment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the experiment.
startExperimentResponse_experiment :: Lens.Lens' StartExperimentResponse (Prelude.Maybe Experiment)
startExperimentResponse_experiment = Lens.lens (\StartExperimentResponse' {experiment} -> experiment) (\s@StartExperimentResponse' {} a -> s {experiment = a} :: StartExperimentResponse)

-- | The response's http status code.
startExperimentResponse_httpStatus :: Lens.Lens' StartExperimentResponse Prelude.Int
startExperimentResponse_httpStatus = Lens.lens (\StartExperimentResponse' {httpStatus} -> httpStatus) (\s@StartExperimentResponse' {} a -> s {httpStatus = a} :: StartExperimentResponse)

instance Prelude.NFData StartExperimentResponse where
  rnf StartExperimentResponse' {..} =
    Prelude.rnf experiment
      `Prelude.seq` Prelude.rnf httpStatus
