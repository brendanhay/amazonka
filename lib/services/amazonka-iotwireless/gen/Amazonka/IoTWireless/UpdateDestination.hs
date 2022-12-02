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
-- Module      : Amazonka.IoTWireless.UpdateDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties of a destination.
module Amazonka.IoTWireless.UpdateDestination
  ( -- * Creating a Request
    UpdateDestination (..),
    newUpdateDestination,

    -- * Request Lenses
    updateDestination_roleArn,
    updateDestination_description,
    updateDestination_expression,
    updateDestination_expressionType,
    updateDestination_name,

    -- * Destructuring the Response
    UpdateDestinationResponse (..),
    newUpdateDestinationResponse,

    -- * Response Lenses
    updateDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { -- | The ARN of the IAM Role that authorizes the destination.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A new description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new rule name or topic rule to send messages to.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The type of value in @Expression@.
    expressionType :: Prelude.Maybe ExpressionType,
    -- | The new name of the resource.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateDestination_roleArn' - The ARN of the IAM Role that authorizes the destination.
--
-- 'description', 'updateDestination_description' - A new description of the resource.
--
-- 'expression', 'updateDestination_expression' - The new rule name or topic rule to send messages to.
--
-- 'expressionType', 'updateDestination_expressionType' - The type of value in @Expression@.
--
-- 'name', 'updateDestination_name' - The new name of the resource.
newUpdateDestination ::
  -- | 'name'
  Prelude.Text ->
  UpdateDestination
newUpdateDestination pName_ =
  UpdateDestination'
    { roleArn = Prelude.Nothing,
      description = Prelude.Nothing,
      expression = Prelude.Nothing,
      expressionType = Prelude.Nothing,
      name = pName_
    }

-- | The ARN of the IAM Role that authorizes the destination.
updateDestination_roleArn :: Lens.Lens' UpdateDestination (Prelude.Maybe Prelude.Text)
updateDestination_roleArn = Lens.lens (\UpdateDestination' {roleArn} -> roleArn) (\s@UpdateDestination' {} a -> s {roleArn = a} :: UpdateDestination)

-- | A new description of the resource.
updateDestination_description :: Lens.Lens' UpdateDestination (Prelude.Maybe Prelude.Text)
updateDestination_description = Lens.lens (\UpdateDestination' {description} -> description) (\s@UpdateDestination' {} a -> s {description = a} :: UpdateDestination)

-- | The new rule name or topic rule to send messages to.
updateDestination_expression :: Lens.Lens' UpdateDestination (Prelude.Maybe Prelude.Text)
updateDestination_expression = Lens.lens (\UpdateDestination' {expression} -> expression) (\s@UpdateDestination' {} a -> s {expression = a} :: UpdateDestination)

-- | The type of value in @Expression@.
updateDestination_expressionType :: Lens.Lens' UpdateDestination (Prelude.Maybe ExpressionType)
updateDestination_expressionType = Lens.lens (\UpdateDestination' {expressionType} -> expressionType) (\s@UpdateDestination' {} a -> s {expressionType = a} :: UpdateDestination)

-- | The new name of the resource.
updateDestination_name :: Lens.Lens' UpdateDestination Prelude.Text
updateDestination_name = Lens.lens (\UpdateDestination' {name} -> name) (\s@UpdateDestination' {} a -> s {name = a} :: UpdateDestination)

instance Core.AWSRequest UpdateDestination where
  type
    AWSResponse UpdateDestination =
      UpdateDestinationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDestination where
  hashWithSalt _salt UpdateDestination' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` expressionType
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateDestination where
  rnf UpdateDestination' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf expressionType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateDestination where
  toJSON UpdateDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Description" Data..=) Prelude.<$> description,
            ("Expression" Data..=) Prelude.<$> expression,
            ("ExpressionType" Data..=)
              Prelude.<$> expressionType
          ]
      )

instance Data.ToPath UpdateDestination where
  toPath UpdateDestination' {..} =
    Prelude.mconcat ["/destinations/", Data.toBS name]

instance Data.ToQuery UpdateDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDestinationResponse' smart constructor.
data UpdateDestinationResponse = UpdateDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDestinationResponse_httpStatus' - The response's http status code.
newUpdateDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDestinationResponse
newUpdateDestinationResponse pHttpStatus_ =
  UpdateDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDestinationResponse_httpStatus :: Lens.Lens' UpdateDestinationResponse Prelude.Int
updateDestinationResponse_httpStatus = Lens.lens (\UpdateDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateDestinationResponse' {} a -> s {httpStatus = a} :: UpdateDestinationResponse)

instance Prelude.NFData UpdateDestinationResponse where
  rnf UpdateDestinationResponse' {..} =
    Prelude.rnf httpStatus
