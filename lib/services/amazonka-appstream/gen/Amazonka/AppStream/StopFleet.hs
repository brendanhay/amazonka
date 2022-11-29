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
-- Module      : Amazonka.AppStream.StopFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified fleet.
module Amazonka.AppStream.StopFleet
  ( -- * Creating a Request
    StopFleet (..),
    newStopFleet,

    -- * Request Lenses
    stopFleet_name,

    -- * Destructuring the Response
    StopFleetResponse (..),
    newStopFleetResponse,

    -- * Response Lenses
    stopFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopFleet' smart constructor.
data StopFleet = StopFleet'
  { -- | The name of the fleet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopFleet_name' - The name of the fleet.
newStopFleet ::
  -- | 'name'
  Prelude.Text ->
  StopFleet
newStopFleet pName_ = StopFleet' {name = pName_}

-- | The name of the fleet.
stopFleet_name :: Lens.Lens' StopFleet Prelude.Text
stopFleet_name = Lens.lens (\StopFleet' {name} -> name) (\s@StopFleet' {} a -> s {name = a} :: StopFleet)

instance Core.AWSRequest StopFleet where
  type AWSResponse StopFleet = StopFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopFleet where
  hashWithSalt _salt StopFleet' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StopFleet where
  rnf StopFleet' {..} = Prelude.rnf name

instance Core.ToHeaders StopFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.StopFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopFleet where
  toJSON StopFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath StopFleet where
  toPath = Prelude.const "/"

instance Core.ToQuery StopFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopFleetResponse' smart constructor.
data StopFleetResponse = StopFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopFleetResponse_httpStatus' - The response's http status code.
newStopFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopFleetResponse
newStopFleetResponse pHttpStatus_ =
  StopFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopFleetResponse_httpStatus :: Lens.Lens' StopFleetResponse Prelude.Int
stopFleetResponse_httpStatus = Lens.lens (\StopFleetResponse' {httpStatus} -> httpStatus) (\s@StopFleetResponse' {} a -> s {httpStatus = a} :: StopFleetResponse)

instance Prelude.NFData StopFleetResponse where
  rnf StopFleetResponse' {..} = Prelude.rnf httpStatus
