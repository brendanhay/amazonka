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
-- Module      : Network.AWS.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorks.GetHostnameSuggestion
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetHostnameSuggestion' smart constructor.
data GetHostnameSuggestion = GetHostnameSuggestion'
  { -- | The layer ID.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetHostnameSuggestion where
  type
    Rs GetHostnameSuggestion =
      GetHostnameSuggestionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHostnameSuggestionResponse'
            Prelude.<$> (x Prelude..?> "Hostname")
            Prelude.<*> (x Prelude..?> "LayerId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHostnameSuggestion

instance Prelude.NFData GetHostnameSuggestion

instance Prelude.ToHeaders GetHostnameSuggestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.GetHostnameSuggestion" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetHostnameSuggestion where
  toJSON GetHostnameSuggestion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("LayerId" Prelude..= layerId)]
      )

instance Prelude.ToPath GetHostnameSuggestion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetHostnameSuggestion where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetHostnameSuggestionResponse
