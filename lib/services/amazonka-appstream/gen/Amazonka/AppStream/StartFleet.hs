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
-- Module      : Amazonka.AppStream.StartFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified fleet.
module Amazonka.AppStream.StartFleet
  ( -- * Creating a Request
    StartFleet (..),
    newStartFleet,

    -- * Request Lenses
    startFleet_name,

    -- * Destructuring the Response
    StartFleetResponse (..),
    newStartFleetResponse,

    -- * Response Lenses
    startFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFleet' smart constructor.
data StartFleet = StartFleet'
  { -- | The name of the fleet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startFleet_name' - The name of the fleet.
newStartFleet ::
  -- | 'name'
  Prelude.Text ->
  StartFleet
newStartFleet pName_ = StartFleet' {name = pName_}

-- | The name of the fleet.
startFleet_name :: Lens.Lens' StartFleet Prelude.Text
startFleet_name = Lens.lens (\StartFleet' {name} -> name) (\s@StartFleet' {} a -> s {name = a} :: StartFleet)

instance Core.AWSRequest StartFleet where
  type AWSResponse StartFleet = StartFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFleet where
  hashWithSalt _salt StartFleet' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StartFleet where
  rnf StartFleet' {..} = Prelude.rnf name

instance Data.ToHeaders StartFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.StartFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFleet where
  toJSON StartFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath StartFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFleetResponse' smart constructor.
data StartFleetResponse = StartFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startFleetResponse_httpStatus' - The response's http status code.
newStartFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFleetResponse
newStartFleetResponse pHttpStatus_ =
  StartFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startFleetResponse_httpStatus :: Lens.Lens' StartFleetResponse Prelude.Int
startFleetResponse_httpStatus = Lens.lens (\StartFleetResponse' {httpStatus} -> httpStatus) (\s@StartFleetResponse' {} a -> s {httpStatus = a} :: StartFleetResponse)

instance Prelude.NFData StartFleetResponse where
  rnf StartFleetResponse' {..} = Prelude.rnf httpStatus
