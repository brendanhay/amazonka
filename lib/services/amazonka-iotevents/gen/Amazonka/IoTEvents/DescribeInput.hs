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
-- Module      : Amazonka.IoTEvents.DescribeInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an input.
module Amazonka.IoTEvents.DescribeInput
  ( -- * Creating a Request
    DescribeInput (..),
    newDescribeInput,

    -- * Request Lenses
    describeInput_inputName,

    -- * Destructuring the Response
    DescribeInputResponse (..),
    newDescribeInputResponse,

    -- * Response Lenses
    describeInputResponse_input,
    describeInputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInput' smart constructor.
data DescribeInput = DescribeInput'
  { -- | The name of the input.
    inputName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputName', 'describeInput_inputName' - The name of the input.
newDescribeInput ::
  -- | 'inputName'
  Prelude.Text ->
  DescribeInput
newDescribeInput pInputName_ =
  DescribeInput' {inputName = pInputName_}

-- | The name of the input.
describeInput_inputName :: Lens.Lens' DescribeInput Prelude.Text
describeInput_inputName = Lens.lens (\DescribeInput' {inputName} -> inputName) (\s@DescribeInput' {} a -> s {inputName = a} :: DescribeInput)

instance Core.AWSRequest DescribeInput where
  type
    AWSResponse DescribeInput =
      DescribeInputResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputResponse'
            Prelude.<$> (x Core..?> "input")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInput where
  hashWithSalt _salt DescribeInput' {..} =
    _salt `Prelude.hashWithSalt` inputName

instance Prelude.NFData DescribeInput where
  rnf DescribeInput' {..} = Prelude.rnf inputName

instance Core.ToHeaders DescribeInput where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInput where
  toPath DescribeInput' {..} =
    Prelude.mconcat ["/inputs/", Core.toBS inputName]

instance Core.ToQuery DescribeInput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'
  { -- | Information about the input.
    input :: Prelude.Maybe Input,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'describeInputResponse_input' - Information about the input.
--
-- 'httpStatus', 'describeInputResponse_httpStatus' - The response's http status code.
newDescribeInputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInputResponse
newDescribeInputResponse pHttpStatus_ =
  DescribeInputResponse'
    { input = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the input.
describeInputResponse_input :: Lens.Lens' DescribeInputResponse (Prelude.Maybe Input)
describeInputResponse_input = Lens.lens (\DescribeInputResponse' {input} -> input) (\s@DescribeInputResponse' {} a -> s {input = a} :: DescribeInputResponse)

-- | The response's http status code.
describeInputResponse_httpStatus :: Lens.Lens' DescribeInputResponse Prelude.Int
describeInputResponse_httpStatus = Lens.lens (\DescribeInputResponse' {httpStatus} -> httpStatus) (\s@DescribeInputResponse' {} a -> s {httpStatus = a} :: DescribeInputResponse)

instance Prelude.NFData DescribeInputResponse where
  rnf DescribeInputResponse' {..} =
    Prelude.rnf input
      `Prelude.seq` Prelude.rnf httpStatus
