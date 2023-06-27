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
-- Module      : Amazonka.Shield.UpdateApplicationLayerAutomaticResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Shield Advanced automatic application layer DDoS
-- mitigation configuration for the specified resource.
module Amazonka.Shield.UpdateApplicationLayerAutomaticResponse
  ( -- * Creating a Request
    UpdateApplicationLayerAutomaticResponse (..),
    newUpdateApplicationLayerAutomaticResponse,

    -- * Request Lenses
    updateApplicationLayerAutomaticResponse_resourceArn,
    updateApplicationLayerAutomaticResponse_action,

    -- * Destructuring the Response
    UpdateApplicationLayerAutomaticResponseResponse (..),
    newUpdateApplicationLayerAutomaticResponseResponse,

    -- * Response Lenses
    updateApplicationLayerAutomaticResponseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newUpdateApplicationLayerAutomaticResponse' smart constructor.
data UpdateApplicationLayerAutomaticResponse = UpdateApplicationLayerAutomaticResponse'
  { -- | The ARN (Amazon Resource Name) of the resource.
    resourceArn :: Prelude.Text,
    -- | Specifies the action setting that Shield Advanced should use in the WAF
    -- rules that it creates on behalf of the protected resource in response to
    -- DDoS attacks. You specify this as part of the configuration for the
    -- automatic application layer DDoS mitigation feature, when you enable or
    -- update automatic mitigation. Shield Advanced creates the WAF rules in a
    -- Shield Advanced-managed rule group, inside the web ACL that you have
    -- associated with the resource.
    action :: ResponseAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationLayerAutomaticResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'updateApplicationLayerAutomaticResponse_resourceArn' - The ARN (Amazon Resource Name) of the resource.
--
-- 'action', 'updateApplicationLayerAutomaticResponse_action' - Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
newUpdateApplicationLayerAutomaticResponse ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'action'
  ResponseAction ->
  UpdateApplicationLayerAutomaticResponse
newUpdateApplicationLayerAutomaticResponse
  pResourceArn_
  pAction_ =
    UpdateApplicationLayerAutomaticResponse'
      { resourceArn =
          pResourceArn_,
        action = pAction_
      }

-- | The ARN (Amazon Resource Name) of the resource.
updateApplicationLayerAutomaticResponse_resourceArn :: Lens.Lens' UpdateApplicationLayerAutomaticResponse Prelude.Text
updateApplicationLayerAutomaticResponse_resourceArn = Lens.lens (\UpdateApplicationLayerAutomaticResponse' {resourceArn} -> resourceArn) (\s@UpdateApplicationLayerAutomaticResponse' {} a -> s {resourceArn = a} :: UpdateApplicationLayerAutomaticResponse)

-- | Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
updateApplicationLayerAutomaticResponse_action :: Lens.Lens' UpdateApplicationLayerAutomaticResponse ResponseAction
updateApplicationLayerAutomaticResponse_action = Lens.lens (\UpdateApplicationLayerAutomaticResponse' {action} -> action) (\s@UpdateApplicationLayerAutomaticResponse' {} a -> s {action = a} :: UpdateApplicationLayerAutomaticResponse)

instance
  Core.AWSRequest
    UpdateApplicationLayerAutomaticResponse
  where
  type
    AWSResponse
      UpdateApplicationLayerAutomaticResponse =
      UpdateApplicationLayerAutomaticResponseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateApplicationLayerAutomaticResponseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateApplicationLayerAutomaticResponse
  where
  hashWithSalt
    _salt
    UpdateApplicationLayerAutomaticResponse' {..} =
      _salt
        `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    UpdateApplicationLayerAutomaticResponse
  where
  rnf UpdateApplicationLayerAutomaticResponse' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf action

instance
  Data.ToHeaders
    UpdateApplicationLayerAutomaticResponse
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.UpdateApplicationLayerAutomaticResponse" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateApplicationLayerAutomaticResponse
  where
  toJSON UpdateApplicationLayerAutomaticResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Action" Data..= action)
          ]
      )

instance
  Data.ToPath
    UpdateApplicationLayerAutomaticResponse
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateApplicationLayerAutomaticResponse
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationLayerAutomaticResponseResponse' smart constructor.
data UpdateApplicationLayerAutomaticResponseResponse = UpdateApplicationLayerAutomaticResponseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationLayerAutomaticResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationLayerAutomaticResponseResponse_httpStatus' - The response's http status code.
newUpdateApplicationLayerAutomaticResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationLayerAutomaticResponseResponse
newUpdateApplicationLayerAutomaticResponseResponse
  pHttpStatus_ =
    UpdateApplicationLayerAutomaticResponseResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateApplicationLayerAutomaticResponseResponse_httpStatus :: Lens.Lens' UpdateApplicationLayerAutomaticResponseResponse Prelude.Int
updateApplicationLayerAutomaticResponseResponse_httpStatus = Lens.lens (\UpdateApplicationLayerAutomaticResponseResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationLayerAutomaticResponseResponse' {} a -> s {httpStatus = a} :: UpdateApplicationLayerAutomaticResponseResponse)

instance
  Prelude.NFData
    UpdateApplicationLayerAutomaticResponseResponse
  where
  rnf
    UpdateApplicationLayerAutomaticResponseResponse' {..} =
      Prelude.rnf httpStatus
