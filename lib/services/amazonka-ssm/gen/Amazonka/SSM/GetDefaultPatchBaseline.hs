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
-- Module      : Amazonka.SSM.GetDefaultPatchBaseline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the default patch baseline. Amazon Web Services Systems
-- Manager supports creating multiple default patch baselines. For example,
-- you can create a default patch baseline for each operating system.
--
-- If you don\'t specify an operating system value, the default patch
-- baseline for Windows is returned.
module Amazonka.SSM.GetDefaultPatchBaseline
  ( -- * Creating a Request
    GetDefaultPatchBaseline (..),
    newGetDefaultPatchBaseline,

    -- * Request Lenses
    getDefaultPatchBaseline_operatingSystem,

    -- * Destructuring the Response
    GetDefaultPatchBaselineResponse (..),
    newGetDefaultPatchBaselineResponse,

    -- * Response Lenses
    getDefaultPatchBaselineResponse_baselineId,
    getDefaultPatchBaselineResponse_operatingSystem,
    getDefaultPatchBaselineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetDefaultPatchBaseline' smart constructor.
data GetDefaultPatchBaseline = GetDefaultPatchBaseline'
  { -- | Returns the default patch baseline for the specified operating system.
    operatingSystem :: Prelude.Maybe OperatingSystem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDefaultPatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystem', 'getDefaultPatchBaseline_operatingSystem' - Returns the default patch baseline for the specified operating system.
newGetDefaultPatchBaseline ::
  GetDefaultPatchBaseline
newGetDefaultPatchBaseline =
  GetDefaultPatchBaseline'
    { operatingSystem =
        Prelude.Nothing
    }

-- | Returns the default patch baseline for the specified operating system.
getDefaultPatchBaseline_operatingSystem :: Lens.Lens' GetDefaultPatchBaseline (Prelude.Maybe OperatingSystem)
getDefaultPatchBaseline_operatingSystem = Lens.lens (\GetDefaultPatchBaseline' {operatingSystem} -> operatingSystem) (\s@GetDefaultPatchBaseline' {} a -> s {operatingSystem = a} :: GetDefaultPatchBaseline)

instance Core.AWSRequest GetDefaultPatchBaseline where
  type
    AWSResponse GetDefaultPatchBaseline =
      GetDefaultPatchBaselineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDefaultPatchBaselineResponse'
            Prelude.<$> (x Data..?> "BaselineId")
            Prelude.<*> (x Data..?> "OperatingSystem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDefaultPatchBaseline where
  hashWithSalt _salt GetDefaultPatchBaseline' {..} =
    _salt `Prelude.hashWithSalt` operatingSystem

instance Prelude.NFData GetDefaultPatchBaseline where
  rnf GetDefaultPatchBaseline' {..} =
    Prelude.rnf operatingSystem

instance Data.ToHeaders GetDefaultPatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetDefaultPatchBaseline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDefaultPatchBaseline where
  toJSON GetDefaultPatchBaseline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OperatingSystem" Data..=)
              Prelude.<$> operatingSystem
          ]
      )

instance Data.ToPath GetDefaultPatchBaseline where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDefaultPatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDefaultPatchBaselineResponse' smart constructor.
data GetDefaultPatchBaselineResponse = GetDefaultPatchBaselineResponse'
  { -- | The ID of the default patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The operating system for the returned patch baseline.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDefaultPatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'getDefaultPatchBaselineResponse_baselineId' - The ID of the default patch baseline.
--
-- 'operatingSystem', 'getDefaultPatchBaselineResponse_operatingSystem' - The operating system for the returned patch baseline.
--
-- 'httpStatus', 'getDefaultPatchBaselineResponse_httpStatus' - The response's http status code.
newGetDefaultPatchBaselineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDefaultPatchBaselineResponse
newGetDefaultPatchBaselineResponse pHttpStatus_ =
  GetDefaultPatchBaselineResponse'
    { baselineId =
        Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the default patch baseline.
getDefaultPatchBaselineResponse_baselineId :: Lens.Lens' GetDefaultPatchBaselineResponse (Prelude.Maybe Prelude.Text)
getDefaultPatchBaselineResponse_baselineId = Lens.lens (\GetDefaultPatchBaselineResponse' {baselineId} -> baselineId) (\s@GetDefaultPatchBaselineResponse' {} a -> s {baselineId = a} :: GetDefaultPatchBaselineResponse)

-- | The operating system for the returned patch baseline.
getDefaultPatchBaselineResponse_operatingSystem :: Lens.Lens' GetDefaultPatchBaselineResponse (Prelude.Maybe OperatingSystem)
getDefaultPatchBaselineResponse_operatingSystem = Lens.lens (\GetDefaultPatchBaselineResponse' {operatingSystem} -> operatingSystem) (\s@GetDefaultPatchBaselineResponse' {} a -> s {operatingSystem = a} :: GetDefaultPatchBaselineResponse)

-- | The response's http status code.
getDefaultPatchBaselineResponse_httpStatus :: Lens.Lens' GetDefaultPatchBaselineResponse Prelude.Int
getDefaultPatchBaselineResponse_httpStatus = Lens.lens (\GetDefaultPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@GetDefaultPatchBaselineResponse' {} a -> s {httpStatus = a} :: GetDefaultPatchBaselineResponse)

instance
  Prelude.NFData
    GetDefaultPatchBaselineResponse
  where
  rnf GetDefaultPatchBaselineResponse' {..} =
    Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf httpStatus
