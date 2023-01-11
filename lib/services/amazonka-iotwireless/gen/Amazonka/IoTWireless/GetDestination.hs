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
-- Module      : Amazonka.IoTWireless.GetDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a destination.
module Amazonka.IoTWireless.GetDestination
  ( -- * Creating a Request
    GetDestination (..),
    newGetDestination,

    -- * Request Lenses
    getDestination_name,

    -- * Destructuring the Response
    GetDestinationResponse (..),
    newGetDestinationResponse,

    -- * Response Lenses
    getDestinationResponse_arn,
    getDestinationResponse_description,
    getDestinationResponse_expression,
    getDestinationResponse_expressionType,
    getDestinationResponse_name,
    getDestinationResponse_roleArn,
    getDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDestination' smart constructor.
data GetDestination = GetDestination'
  { -- | The name of the resource to get.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getDestination_name' - The name of the resource to get.
newGetDestination ::
  -- | 'name'
  Prelude.Text ->
  GetDestination
newGetDestination pName_ =
  GetDestination' {name = pName_}

-- | The name of the resource to get.
getDestination_name :: Lens.Lens' GetDestination Prelude.Text
getDestination_name = Lens.lens (\GetDestination' {name} -> name) (\s@GetDestination' {} a -> s {name = a} :: GetDestination)

instance Core.AWSRequest GetDestination where
  type
    AWSResponse GetDestination =
      GetDestinationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDestinationResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Expression")
            Prelude.<*> (x Data..?> "ExpressionType")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDestination where
  hashWithSalt _salt GetDestination' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetDestination where
  rnf GetDestination' {..} = Prelude.rnf name

instance Data.ToHeaders GetDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDestination where
  toPath GetDestination' {..} =
    Prelude.mconcat ["/destinations/", Data.toBS name]

instance Data.ToQuery GetDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDestinationResponse' smart constructor.
data GetDestinationResponse = GetDestinationResponse'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The rule name or topic rule to send messages to.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The type of value in @Expression@.
    expressionType :: Prelude.Maybe ExpressionType,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM Role that authorizes the destination.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDestinationResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'description', 'getDestinationResponse_description' - The description of the resource.
--
-- 'expression', 'getDestinationResponse_expression' - The rule name or topic rule to send messages to.
--
-- 'expressionType', 'getDestinationResponse_expressionType' - The type of value in @Expression@.
--
-- 'name', 'getDestinationResponse_name' - The name of the resource.
--
-- 'roleArn', 'getDestinationResponse_roleArn' - The ARN of the IAM Role that authorizes the destination.
--
-- 'httpStatus', 'getDestinationResponse_httpStatus' - The response's http status code.
newGetDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDestinationResponse
newGetDestinationResponse pHttpStatus_ =
  GetDestinationResponse'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      expression = Prelude.Nothing,
      expressionType = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the resource.
getDestinationResponse_arn :: Lens.Lens' GetDestinationResponse (Prelude.Maybe Prelude.Text)
getDestinationResponse_arn = Lens.lens (\GetDestinationResponse' {arn} -> arn) (\s@GetDestinationResponse' {} a -> s {arn = a} :: GetDestinationResponse)

-- | The description of the resource.
getDestinationResponse_description :: Lens.Lens' GetDestinationResponse (Prelude.Maybe Prelude.Text)
getDestinationResponse_description = Lens.lens (\GetDestinationResponse' {description} -> description) (\s@GetDestinationResponse' {} a -> s {description = a} :: GetDestinationResponse)

-- | The rule name or topic rule to send messages to.
getDestinationResponse_expression :: Lens.Lens' GetDestinationResponse (Prelude.Maybe Prelude.Text)
getDestinationResponse_expression = Lens.lens (\GetDestinationResponse' {expression} -> expression) (\s@GetDestinationResponse' {} a -> s {expression = a} :: GetDestinationResponse)

-- | The type of value in @Expression@.
getDestinationResponse_expressionType :: Lens.Lens' GetDestinationResponse (Prelude.Maybe ExpressionType)
getDestinationResponse_expressionType = Lens.lens (\GetDestinationResponse' {expressionType} -> expressionType) (\s@GetDestinationResponse' {} a -> s {expressionType = a} :: GetDestinationResponse)

-- | The name of the resource.
getDestinationResponse_name :: Lens.Lens' GetDestinationResponse (Prelude.Maybe Prelude.Text)
getDestinationResponse_name = Lens.lens (\GetDestinationResponse' {name} -> name) (\s@GetDestinationResponse' {} a -> s {name = a} :: GetDestinationResponse)

-- | The ARN of the IAM Role that authorizes the destination.
getDestinationResponse_roleArn :: Lens.Lens' GetDestinationResponse (Prelude.Maybe Prelude.Text)
getDestinationResponse_roleArn = Lens.lens (\GetDestinationResponse' {roleArn} -> roleArn) (\s@GetDestinationResponse' {} a -> s {roleArn = a} :: GetDestinationResponse)

-- | The response's http status code.
getDestinationResponse_httpStatus :: Lens.Lens' GetDestinationResponse Prelude.Int
getDestinationResponse_httpStatus = Lens.lens (\GetDestinationResponse' {httpStatus} -> httpStatus) (\s@GetDestinationResponse' {} a -> s {httpStatus = a} :: GetDestinationResponse)

instance Prelude.NFData GetDestinationResponse where
  rnf GetDestinationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf expressionType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
