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
-- Module      : Network.AWS.AppStream.StartFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified fleet.
module Network.AWS.AppStream.StartFleet
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartFleet' smart constructor.
data StartFleet = StartFleet'
  { -- | The name of the fleet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StartFleet where
  type Rs StartFleet = StartFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFleet

instance Prelude.NFData StartFleet

instance Prelude.ToHeaders StartFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.StartFleet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartFleet where
  toJSON StartFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StartFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFleetResponse' smart constructor.
data StartFleetResponse = StartFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StartFleetResponse
