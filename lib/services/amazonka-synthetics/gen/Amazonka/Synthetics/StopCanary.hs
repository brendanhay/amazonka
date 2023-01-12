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
-- Module      : Amazonka.Synthetics.StopCanary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the canary to prevent all future runs. If the canary is currently
-- running,the run that is in progress completes on its own, publishes
-- metrics, and uploads artifacts, but it is not recorded in Synthetics as
-- a completed run.
--
-- You can use @StartCanary@ to start it running again with the canary’s
-- current schedule at any point in the future.
module Amazonka.Synthetics.StopCanary
  ( -- * Creating a Request
    StopCanary (..),
    newStopCanary,

    -- * Request Lenses
    stopCanary_name,

    -- * Destructuring the Response
    StopCanaryResponse (..),
    newStopCanaryResponse,

    -- * Response Lenses
    stopCanaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newStopCanary' smart constructor.
data StopCanary = StopCanary'
  { -- | The name of the canary that you want to stop. To find the names of your
    -- canaries, use
    -- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html ListCanaries>.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCanary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopCanary_name' - The name of the canary that you want to stop. To find the names of your
-- canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html ListCanaries>.
newStopCanary ::
  -- | 'name'
  Prelude.Text ->
  StopCanary
newStopCanary pName_ = StopCanary' {name = pName_}

-- | The name of the canary that you want to stop. To find the names of your
-- canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html ListCanaries>.
stopCanary_name :: Lens.Lens' StopCanary Prelude.Text
stopCanary_name = Lens.lens (\StopCanary' {name} -> name) (\s@StopCanary' {} a -> s {name = a} :: StopCanary)

instance Core.AWSRequest StopCanary where
  type AWSResponse StopCanary = StopCanaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCanaryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopCanary where
  hashWithSalt _salt StopCanary' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StopCanary where
  rnf StopCanary' {..} = Prelude.rnf name

instance Data.ToHeaders StopCanary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopCanary where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopCanary where
  toPath StopCanary' {..} =
    Prelude.mconcat
      ["/canary/", Data.toBS name, "/stop"]

instance Data.ToQuery StopCanary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCanaryResponse' smart constructor.
data StopCanaryResponse = StopCanaryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCanaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopCanaryResponse_httpStatus' - The response's http status code.
newStopCanaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopCanaryResponse
newStopCanaryResponse pHttpStatus_ =
  StopCanaryResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopCanaryResponse_httpStatus :: Lens.Lens' StopCanaryResponse Prelude.Int
stopCanaryResponse_httpStatus = Lens.lens (\StopCanaryResponse' {httpStatus} -> httpStatus) (\s@StopCanaryResponse' {} a -> s {httpStatus = a} :: StopCanaryResponse)

instance Prelude.NFData StopCanaryResponse where
  rnf StopCanaryResponse' {..} = Prelude.rnf httpStatus
