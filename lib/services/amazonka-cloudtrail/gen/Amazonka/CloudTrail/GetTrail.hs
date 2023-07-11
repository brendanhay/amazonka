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
-- Module      : Amazonka.CloudTrail.GetTrail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns settings information for a specified trail.
module Amazonka.CloudTrail.GetTrail
  ( -- * Creating a Request
    GetTrail (..),
    newGetTrail,

    -- * Request Lenses
    getTrail_name,

    -- * Destructuring the Response
    GetTrailResponse (..),
    newGetTrailResponse,

    -- * Response Lenses
    getTrailResponse_trail,
    getTrailResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTrail' smart constructor.
data GetTrail = GetTrail'
  { -- | The name or the Amazon Resource Name (ARN) of the trail for which you
    -- want to retrieve settings information.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getTrail_name' - The name or the Amazon Resource Name (ARN) of the trail for which you
-- want to retrieve settings information.
newGetTrail ::
  -- | 'name'
  Prelude.Text ->
  GetTrail
newGetTrail pName_ = GetTrail' {name = pName_}

-- | The name or the Amazon Resource Name (ARN) of the trail for which you
-- want to retrieve settings information.
getTrail_name :: Lens.Lens' GetTrail Prelude.Text
getTrail_name = Lens.lens (\GetTrail' {name} -> name) (\s@GetTrail' {} a -> s {name = a} :: GetTrail)

instance Core.AWSRequest GetTrail where
  type AWSResponse GetTrail = GetTrailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrailResponse'
            Prelude.<$> (x Data..?> "Trail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrail where
  hashWithSalt _salt GetTrail' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetTrail where
  rnf GetTrail' {..} = Prelude.rnf name

instance Data.ToHeaders GetTrail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTrail where
  toJSON GetTrail' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetTrail where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTrail where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTrailResponse' smart constructor.
data GetTrailResponse = GetTrailResponse'
  { trail :: Prelude.Maybe Trail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trail', 'getTrailResponse_trail' - Undocumented member.
--
-- 'httpStatus', 'getTrailResponse_httpStatus' - The response's http status code.
newGetTrailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTrailResponse
newGetTrailResponse pHttpStatus_ =
  GetTrailResponse'
    { trail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getTrailResponse_trail :: Lens.Lens' GetTrailResponse (Prelude.Maybe Trail)
getTrailResponse_trail = Lens.lens (\GetTrailResponse' {trail} -> trail) (\s@GetTrailResponse' {} a -> s {trail = a} :: GetTrailResponse)

-- | The response's http status code.
getTrailResponse_httpStatus :: Lens.Lens' GetTrailResponse Prelude.Int
getTrailResponse_httpStatus = Lens.lens (\GetTrailResponse' {httpStatus} -> httpStatus) (\s@GetTrailResponse' {} a -> s {httpStatus = a} :: GetTrailResponse)

instance Prelude.NFData GetTrailResponse where
  rnf GetTrailResponse' {..} =
    Prelude.rnf trail
      `Prelude.seq` Prelude.rnf httpStatus
