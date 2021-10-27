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
-- Module      : Network.AWS.IoTEvents.DescribeInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an input.
module Network.AWS.IoTEvents.DescribeInput
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputResponse'
            Prelude.<$> (x Core..?> "input")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInput

instance Prelude.NFData DescribeInput

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

instance Prelude.NFData DescribeInputResponse
