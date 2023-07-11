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
-- Module      : Amazonka.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a generated host name for the specified layer, based on the current
-- host name theme.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.GetHostnameSuggestion
  ( -- * Creating a Request
    GetHostnameSuggestion (..),
    newGetHostnameSuggestion,

    -- * Request Lenses
    getHostnameSuggestion_layerId,

    -- * Destructuring the Response
    GetHostnameSuggestionResponse (..),
    newGetHostnameSuggestionResponse,

    -- * Response Lenses
    getHostnameSuggestionResponse_hostname,
    getHostnameSuggestionResponse_layerId,
    getHostnameSuggestionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHostnameSuggestion' smart constructor.
data GetHostnameSuggestion = GetHostnameSuggestion'
  { -- | The layer ID.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostnameSuggestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerId', 'getHostnameSuggestion_layerId' - The layer ID.
newGetHostnameSuggestion ::
  -- | 'layerId'
  Prelude.Text ->
  GetHostnameSuggestion
newGetHostnameSuggestion pLayerId_ =
  GetHostnameSuggestion' {layerId = pLayerId_}

-- | The layer ID.
getHostnameSuggestion_layerId :: Lens.Lens' GetHostnameSuggestion Prelude.Text
getHostnameSuggestion_layerId = Lens.lens (\GetHostnameSuggestion' {layerId} -> layerId) (\s@GetHostnameSuggestion' {} a -> s {layerId = a} :: GetHostnameSuggestion)

instance Core.AWSRequest GetHostnameSuggestion where
  type
    AWSResponse GetHostnameSuggestion =
      GetHostnameSuggestionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHostnameSuggestionResponse'
            Prelude.<$> (x Data..?> "Hostname")
            Prelude.<*> (x Data..?> "LayerId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHostnameSuggestion where
  hashWithSalt _salt GetHostnameSuggestion' {..} =
    _salt `Prelude.hashWithSalt` layerId

instance Prelude.NFData GetHostnameSuggestion where
  rnf GetHostnameSuggestion' {..} = Prelude.rnf layerId

instance Data.ToHeaders GetHostnameSuggestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.GetHostnameSuggestion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetHostnameSuggestion where
  toJSON GetHostnameSuggestion' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LayerId" Data..= layerId)]
      )

instance Data.ToPath GetHostnameSuggestion where
  toPath = Prelude.const "/"

instance Data.ToQuery GetHostnameSuggestion where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @GetHostnameSuggestion@ request.
--
-- /See:/ 'newGetHostnameSuggestionResponse' smart constructor.
data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse'
  { -- | The generated host name.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The layer ID.
    layerId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostnameSuggestionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'getHostnameSuggestionResponse_hostname' - The generated host name.
--
-- 'layerId', 'getHostnameSuggestionResponse_layerId' - The layer ID.
--
-- 'httpStatus', 'getHostnameSuggestionResponse_httpStatus' - The response's http status code.
newGetHostnameSuggestionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHostnameSuggestionResponse
newGetHostnameSuggestionResponse pHttpStatus_ =
  GetHostnameSuggestionResponse'
    { hostname =
        Prelude.Nothing,
      layerId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The generated host name.
getHostnameSuggestionResponse_hostname :: Lens.Lens' GetHostnameSuggestionResponse (Prelude.Maybe Prelude.Text)
getHostnameSuggestionResponse_hostname = Lens.lens (\GetHostnameSuggestionResponse' {hostname} -> hostname) (\s@GetHostnameSuggestionResponse' {} a -> s {hostname = a} :: GetHostnameSuggestionResponse)

-- | The layer ID.
getHostnameSuggestionResponse_layerId :: Lens.Lens' GetHostnameSuggestionResponse (Prelude.Maybe Prelude.Text)
getHostnameSuggestionResponse_layerId = Lens.lens (\GetHostnameSuggestionResponse' {layerId} -> layerId) (\s@GetHostnameSuggestionResponse' {} a -> s {layerId = a} :: GetHostnameSuggestionResponse)

-- | The response's http status code.
getHostnameSuggestionResponse_httpStatus :: Lens.Lens' GetHostnameSuggestionResponse Prelude.Int
getHostnameSuggestionResponse_httpStatus = Lens.lens (\GetHostnameSuggestionResponse' {httpStatus} -> httpStatus) (\s@GetHostnameSuggestionResponse' {} a -> s {httpStatus = a} :: GetHostnameSuggestionResponse)

instance Prelude.NFData GetHostnameSuggestionResponse where
  rnf GetHostnameSuggestionResponse' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf layerId
      `Prelude.seq` Prelude.rnf httpStatus
