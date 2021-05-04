{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.StopFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified fleet.
module Network.AWS.AppStream.StopFleet
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopFleet' smart constructor.
data StopFleet = StopFleet'
  { -- | The name of the fleet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopFleet where
  type Rs StopFleet = StopFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopFleet

instance Prelude.NFData StopFleet

instance Prelude.ToHeaders StopFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.StopFleet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopFleet where
  toJSON StopFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StopFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopFleetResponse' smart constructor.
data StopFleetResponse = StopFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StopFleetResponse
