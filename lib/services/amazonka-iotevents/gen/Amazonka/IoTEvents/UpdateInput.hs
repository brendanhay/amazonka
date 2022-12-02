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
-- Module      : Amazonka.IoTEvents.UpdateInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an input.
module Amazonka.IoTEvents.UpdateInput
  ( -- * Creating a Request
    UpdateInput (..),
    newUpdateInput,

    -- * Request Lenses
    updateInput_inputDescription,
    updateInput_inputName,
    updateInput_inputDefinition,

    -- * Destructuring the Response
    UpdateInputResponse (..),
    newUpdateInputResponse,

    -- * Response Lenses
    updateInputResponse_inputConfiguration,
    updateInputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInput' smart constructor.
data UpdateInput = UpdateInput'
  { -- | A brief description of the input.
    inputDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the input you want to update.
    inputName :: Prelude.Text,
    -- | The definition of the input.
    inputDefinition :: InputDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDescription', 'updateInput_inputDescription' - A brief description of the input.
--
-- 'inputName', 'updateInput_inputName' - The name of the input you want to update.
--
-- 'inputDefinition', 'updateInput_inputDefinition' - The definition of the input.
newUpdateInput ::
  -- | 'inputName'
  Prelude.Text ->
  -- | 'inputDefinition'
  InputDefinition ->
  UpdateInput
newUpdateInput pInputName_ pInputDefinition_ =
  UpdateInput'
    { inputDescription = Prelude.Nothing,
      inputName = pInputName_,
      inputDefinition = pInputDefinition_
    }

-- | A brief description of the input.
updateInput_inputDescription :: Lens.Lens' UpdateInput (Prelude.Maybe Prelude.Text)
updateInput_inputDescription = Lens.lens (\UpdateInput' {inputDescription} -> inputDescription) (\s@UpdateInput' {} a -> s {inputDescription = a} :: UpdateInput)

-- | The name of the input you want to update.
updateInput_inputName :: Lens.Lens' UpdateInput Prelude.Text
updateInput_inputName = Lens.lens (\UpdateInput' {inputName} -> inputName) (\s@UpdateInput' {} a -> s {inputName = a} :: UpdateInput)

-- | The definition of the input.
updateInput_inputDefinition :: Lens.Lens' UpdateInput InputDefinition
updateInput_inputDefinition = Lens.lens (\UpdateInput' {inputDefinition} -> inputDefinition) (\s@UpdateInput' {} a -> s {inputDefinition = a} :: UpdateInput)

instance Core.AWSRequest UpdateInput where
  type AWSResponse UpdateInput = UpdateInputResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputResponse'
            Prelude.<$> (x Data..?> "inputConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInput where
  hashWithSalt _salt UpdateInput' {..} =
    _salt `Prelude.hashWithSalt` inputDescription
      `Prelude.hashWithSalt` inputName
      `Prelude.hashWithSalt` inputDefinition

instance Prelude.NFData UpdateInput where
  rnf UpdateInput' {..} =
    Prelude.rnf inputDescription
      `Prelude.seq` Prelude.rnf inputName
      `Prelude.seq` Prelude.rnf inputDefinition

instance Data.ToHeaders UpdateInput where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateInput where
  toJSON UpdateInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputDescription" Data..=)
              Prelude.<$> inputDescription,
            Prelude.Just
              ("inputDefinition" Data..= inputDefinition)
          ]
      )

instance Data.ToPath UpdateInput where
  toPath UpdateInput' {..} =
    Prelude.mconcat ["/inputs/", Data.toBS inputName]

instance Data.ToQuery UpdateInput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInputResponse' smart constructor.
data UpdateInputResponse = UpdateInputResponse'
  { -- | Information about the configuration of the input.
    inputConfiguration :: Prelude.Maybe InputConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputConfiguration', 'updateInputResponse_inputConfiguration' - Information about the configuration of the input.
--
-- 'httpStatus', 'updateInputResponse_httpStatus' - The response's http status code.
newUpdateInputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInputResponse
newUpdateInputResponse pHttpStatus_ =
  UpdateInputResponse'
    { inputConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the configuration of the input.
updateInputResponse_inputConfiguration :: Lens.Lens' UpdateInputResponse (Prelude.Maybe InputConfiguration)
updateInputResponse_inputConfiguration = Lens.lens (\UpdateInputResponse' {inputConfiguration} -> inputConfiguration) (\s@UpdateInputResponse' {} a -> s {inputConfiguration = a} :: UpdateInputResponse)

-- | The response's http status code.
updateInputResponse_httpStatus :: Lens.Lens' UpdateInputResponse Prelude.Int
updateInputResponse_httpStatus = Lens.lens (\UpdateInputResponse' {httpStatus} -> httpStatus) (\s@UpdateInputResponse' {} a -> s {httpStatus = a} :: UpdateInputResponse)

instance Prelude.NFData UpdateInputResponse where
  rnf UpdateInputResponse' {..} =
    Prelude.rnf inputConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
