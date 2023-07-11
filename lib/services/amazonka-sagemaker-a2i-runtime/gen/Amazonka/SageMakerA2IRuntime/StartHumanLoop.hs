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
-- Module      : Amazonka.SageMakerA2IRuntime.StartHumanLoop
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a human loop, provided that at least one activation condition is
-- met.
module Amazonka.SageMakerA2IRuntime.StartHumanLoop
  ( -- * Creating a Request
    StartHumanLoop (..),
    newStartHumanLoop,

    -- * Request Lenses
    startHumanLoop_dataAttributes,
    startHumanLoop_humanLoopName,
    startHumanLoop_flowDefinitionArn,
    startHumanLoop_humanLoopInput,

    -- * Destructuring the Response
    StartHumanLoopResponse (..),
    newStartHumanLoopResponse,

    -- * Response Lenses
    startHumanLoopResponse_humanLoopArn,
    startHumanLoopResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerA2IRuntime.Types

-- | /See:/ 'newStartHumanLoop' smart constructor.
data StartHumanLoop = StartHumanLoop'
  { -- | Attributes of the specified data. Use @DataAttributes@ to specify if
    -- your data is free of personally identifiable information and\/or free of
    -- adult content.
    dataAttributes :: Prelude.Maybe HumanLoopDataAttributes,
    -- | The name of the human loop.
    humanLoopName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition associated with
    -- this human loop.
    flowDefinitionArn :: Prelude.Text,
    -- | An object that contains information about the human loop.
    humanLoopInput :: HumanLoopInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartHumanLoop' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAttributes', 'startHumanLoop_dataAttributes' - Attributes of the specified data. Use @DataAttributes@ to specify if
-- your data is free of personally identifiable information and\/or free of
-- adult content.
--
-- 'humanLoopName', 'startHumanLoop_humanLoopName' - The name of the human loop.
--
-- 'flowDefinitionArn', 'startHumanLoop_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition associated with
-- this human loop.
--
-- 'humanLoopInput', 'startHumanLoop_humanLoopInput' - An object that contains information about the human loop.
newStartHumanLoop ::
  -- | 'humanLoopName'
  Prelude.Text ->
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  -- | 'humanLoopInput'
  HumanLoopInput ->
  StartHumanLoop
newStartHumanLoop
  pHumanLoopName_
  pFlowDefinitionArn_
  pHumanLoopInput_ =
    StartHumanLoop'
      { dataAttributes = Prelude.Nothing,
        humanLoopName = pHumanLoopName_,
        flowDefinitionArn = pFlowDefinitionArn_,
        humanLoopInput = pHumanLoopInput_
      }

-- | Attributes of the specified data. Use @DataAttributes@ to specify if
-- your data is free of personally identifiable information and\/or free of
-- adult content.
startHumanLoop_dataAttributes :: Lens.Lens' StartHumanLoop (Prelude.Maybe HumanLoopDataAttributes)
startHumanLoop_dataAttributes = Lens.lens (\StartHumanLoop' {dataAttributes} -> dataAttributes) (\s@StartHumanLoop' {} a -> s {dataAttributes = a} :: StartHumanLoop)

-- | The name of the human loop.
startHumanLoop_humanLoopName :: Lens.Lens' StartHumanLoop Prelude.Text
startHumanLoop_humanLoopName = Lens.lens (\StartHumanLoop' {humanLoopName} -> humanLoopName) (\s@StartHumanLoop' {} a -> s {humanLoopName = a} :: StartHumanLoop)

-- | The Amazon Resource Name (ARN) of the flow definition associated with
-- this human loop.
startHumanLoop_flowDefinitionArn :: Lens.Lens' StartHumanLoop Prelude.Text
startHumanLoop_flowDefinitionArn = Lens.lens (\StartHumanLoop' {flowDefinitionArn} -> flowDefinitionArn) (\s@StartHumanLoop' {} a -> s {flowDefinitionArn = a} :: StartHumanLoop)

-- | An object that contains information about the human loop.
startHumanLoop_humanLoopInput :: Lens.Lens' StartHumanLoop HumanLoopInput
startHumanLoop_humanLoopInput = Lens.lens (\StartHumanLoop' {humanLoopInput} -> humanLoopInput) (\s@StartHumanLoop' {} a -> s {humanLoopInput = a} :: StartHumanLoop)

instance Core.AWSRequest StartHumanLoop where
  type
    AWSResponse StartHumanLoop =
      StartHumanLoopResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartHumanLoopResponse'
            Prelude.<$> (x Data..?> "HumanLoopArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartHumanLoop where
  hashWithSalt _salt StartHumanLoop' {..} =
    _salt
      `Prelude.hashWithSalt` dataAttributes
      `Prelude.hashWithSalt` humanLoopName
      `Prelude.hashWithSalt` flowDefinitionArn
      `Prelude.hashWithSalt` humanLoopInput

instance Prelude.NFData StartHumanLoop where
  rnf StartHumanLoop' {..} =
    Prelude.rnf dataAttributes
      `Prelude.seq` Prelude.rnf humanLoopName
      `Prelude.seq` Prelude.rnf flowDefinitionArn
      `Prelude.seq` Prelude.rnf humanLoopInput

instance Data.ToHeaders StartHumanLoop where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartHumanLoop where
  toJSON StartHumanLoop' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataAttributes" Data..=)
              Prelude.<$> dataAttributes,
            Prelude.Just ("HumanLoopName" Data..= humanLoopName),
            Prelude.Just
              ("FlowDefinitionArn" Data..= flowDefinitionArn),
            Prelude.Just
              ("HumanLoopInput" Data..= humanLoopInput)
          ]
      )

instance Data.ToPath StartHumanLoop where
  toPath = Prelude.const "/human-loops"

instance Data.ToQuery StartHumanLoop where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartHumanLoopResponse' smart constructor.
data StartHumanLoopResponse = StartHumanLoopResponse'
  { -- | The Amazon Resource Name (ARN) of the human loop.
    humanLoopArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartHumanLoopResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopArn', 'startHumanLoopResponse_humanLoopArn' - The Amazon Resource Name (ARN) of the human loop.
--
-- 'httpStatus', 'startHumanLoopResponse_httpStatus' - The response's http status code.
newStartHumanLoopResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartHumanLoopResponse
newStartHumanLoopResponse pHttpStatus_ =
  StartHumanLoopResponse'
    { humanLoopArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the human loop.
startHumanLoopResponse_humanLoopArn :: Lens.Lens' StartHumanLoopResponse (Prelude.Maybe Prelude.Text)
startHumanLoopResponse_humanLoopArn = Lens.lens (\StartHumanLoopResponse' {humanLoopArn} -> humanLoopArn) (\s@StartHumanLoopResponse' {} a -> s {humanLoopArn = a} :: StartHumanLoopResponse)

-- | The response's http status code.
startHumanLoopResponse_httpStatus :: Lens.Lens' StartHumanLoopResponse Prelude.Int
startHumanLoopResponse_httpStatus = Lens.lens (\StartHumanLoopResponse' {httpStatus} -> httpStatus) (\s@StartHumanLoopResponse' {} a -> s {httpStatus = a} :: StartHumanLoopResponse)

instance Prelude.NFData StartHumanLoopResponse where
  rnf StartHumanLoopResponse' {..} =
    Prelude.rnf humanLoopArn
      `Prelude.seq` Prelude.rnf httpStatus
