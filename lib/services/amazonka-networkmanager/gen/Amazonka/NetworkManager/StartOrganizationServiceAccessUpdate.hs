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
-- Module      : Amazonka.NetworkManager.StartOrganizationServiceAccessUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables for the Network Manager service for an Amazon Web Services
-- Organization. This can only be called by a management account within the
-- organization.
module Amazonka.NetworkManager.StartOrganizationServiceAccessUpdate
  ( -- * Creating a Request
    StartOrganizationServiceAccessUpdate (..),
    newStartOrganizationServiceAccessUpdate,

    -- * Request Lenses
    startOrganizationServiceAccessUpdate_action,

    -- * Destructuring the Response
    StartOrganizationServiceAccessUpdateResponse (..),
    newStartOrganizationServiceAccessUpdateResponse,

    -- * Response Lenses
    startOrganizationServiceAccessUpdateResponse_organizationStatus,
    startOrganizationServiceAccessUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartOrganizationServiceAccessUpdate' smart constructor.
data StartOrganizationServiceAccessUpdate = StartOrganizationServiceAccessUpdate'
  { -- | The action to take for the update request. This can be either @ENABLE@
    -- or @DISABLE@.
    action :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOrganizationServiceAccessUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'startOrganizationServiceAccessUpdate_action' - The action to take for the update request. This can be either @ENABLE@
-- or @DISABLE@.
newStartOrganizationServiceAccessUpdate ::
  -- | 'action'
  Prelude.Text ->
  StartOrganizationServiceAccessUpdate
newStartOrganizationServiceAccessUpdate pAction_ =
  StartOrganizationServiceAccessUpdate'
    { action =
        pAction_
    }

-- | The action to take for the update request. This can be either @ENABLE@
-- or @DISABLE@.
startOrganizationServiceAccessUpdate_action :: Lens.Lens' StartOrganizationServiceAccessUpdate Prelude.Text
startOrganizationServiceAccessUpdate_action = Lens.lens (\StartOrganizationServiceAccessUpdate' {action} -> action) (\s@StartOrganizationServiceAccessUpdate' {} a -> s {action = a} :: StartOrganizationServiceAccessUpdate)

instance
  Core.AWSRequest
    StartOrganizationServiceAccessUpdate
  where
  type
    AWSResponse StartOrganizationServiceAccessUpdate =
      StartOrganizationServiceAccessUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOrganizationServiceAccessUpdateResponse'
            Prelude.<$> (x Core..?> "OrganizationStatus")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartOrganizationServiceAccessUpdate
  where
  hashWithSalt
    _salt
    StartOrganizationServiceAccessUpdate' {..} =
      _salt `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    StartOrganizationServiceAccessUpdate
  where
  rnf StartOrganizationServiceAccessUpdate' {..} =
    Prelude.rnf action

instance
  Core.ToHeaders
    StartOrganizationServiceAccessUpdate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    StartOrganizationServiceAccessUpdate
  where
  toJSON StartOrganizationServiceAccessUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Action" Core..= action)]
      )

instance
  Core.ToPath
    StartOrganizationServiceAccessUpdate
  where
  toPath =
    Prelude.const "/organizations/service-access"

instance
  Core.ToQuery
    StartOrganizationServiceAccessUpdate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartOrganizationServiceAccessUpdateResponse' smart constructor.
data StartOrganizationServiceAccessUpdateResponse = StartOrganizationServiceAccessUpdateResponse'
  { -- | The status of the service access update request for an Amazon Web
    -- Services Organization.
    organizationStatus :: Prelude.Maybe OrganizationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOrganizationServiceAccessUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationStatus', 'startOrganizationServiceAccessUpdateResponse_organizationStatus' - The status of the service access update request for an Amazon Web
-- Services Organization.
--
-- 'httpStatus', 'startOrganizationServiceAccessUpdateResponse_httpStatus' - The response's http status code.
newStartOrganizationServiceAccessUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartOrganizationServiceAccessUpdateResponse
newStartOrganizationServiceAccessUpdateResponse
  pHttpStatus_ =
    StartOrganizationServiceAccessUpdateResponse'
      { organizationStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the service access update request for an Amazon Web
-- Services Organization.
startOrganizationServiceAccessUpdateResponse_organizationStatus :: Lens.Lens' StartOrganizationServiceAccessUpdateResponse (Prelude.Maybe OrganizationStatus)
startOrganizationServiceAccessUpdateResponse_organizationStatus = Lens.lens (\StartOrganizationServiceAccessUpdateResponse' {organizationStatus} -> organizationStatus) (\s@StartOrganizationServiceAccessUpdateResponse' {} a -> s {organizationStatus = a} :: StartOrganizationServiceAccessUpdateResponse)

-- | The response's http status code.
startOrganizationServiceAccessUpdateResponse_httpStatus :: Lens.Lens' StartOrganizationServiceAccessUpdateResponse Prelude.Int
startOrganizationServiceAccessUpdateResponse_httpStatus = Lens.lens (\StartOrganizationServiceAccessUpdateResponse' {httpStatus} -> httpStatus) (\s@StartOrganizationServiceAccessUpdateResponse' {} a -> s {httpStatus = a} :: StartOrganizationServiceAccessUpdateResponse)

instance
  Prelude.NFData
    StartOrganizationServiceAccessUpdateResponse
  where
  rnf StartOrganizationServiceAccessUpdateResponse' {..} =
    Prelude.rnf organizationStatus
      `Prelude.seq` Prelude.rnf httpStatus
