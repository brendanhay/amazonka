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
-- Module      : Amazonka.Shield.EnableApplicationLayerAutomaticResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable the Shield Advanced automatic application layer DDoS mitigation
-- for the protected resource.
--
-- This feature is available for Amazon CloudFront distributions and
-- Application Load Balancers only.
--
-- This causes Shield Advanced to create, verify, and apply WAF rules for
-- DDoS attacks that it detects for the resource. Shield Advanced applies
-- the rules in a Shield rule group inside the web ACL that you\'ve
-- associated with the resource. For information about how automatic
-- mitigation works and the requirements for using it, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-advanced-automatic-app-layer-response.html Shield Advanced automatic application layer DDoS mitigation>.
--
-- Don\'t use this action to make changes to automatic mitigation settings
-- when it\'s already enabled for a resource. Instead, use
-- UpdateApplicationLayerAutomaticResponse.
--
-- To use this feature, you must associate a web ACL with the protected
-- resource. The web ACL must be created using the latest version of WAF
-- (v2). You can associate the web ACL through the Shield Advanced console
-- at <https://console.aws.amazon.com/wafv2/shieldv2#/>. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/getting-started-ddos.html Getting Started with Shield Advanced>.
-- You can also associate the web ACL to the resource through the WAF
-- console or the WAF API, but you must manage Shield Advanced automatic
-- mitigation through Shield Advanced. For information about WAF, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ WAF Developer Guide>.
module Amazonka.Shield.EnableApplicationLayerAutomaticResponse
  ( -- * Creating a Request
    EnableApplicationLayerAutomaticResponse (..),
    newEnableApplicationLayerAutomaticResponse,

    -- * Request Lenses
    enableApplicationLayerAutomaticResponse_resourceArn,
    enableApplicationLayerAutomaticResponse_action,

    -- * Destructuring the Response
    EnableApplicationLayerAutomaticResponseResponse (..),
    newEnableApplicationLayerAutomaticResponseResponse,

    -- * Response Lenses
    enableApplicationLayerAutomaticResponseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newEnableApplicationLayerAutomaticResponse' smart constructor.
data EnableApplicationLayerAutomaticResponse = EnableApplicationLayerAutomaticResponse'
  { -- | The ARN (Amazon Resource Name) of the protected resource.
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
-- Create a value of 'EnableApplicationLayerAutomaticResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'enableApplicationLayerAutomaticResponse_resourceArn' - The ARN (Amazon Resource Name) of the protected resource.
--
-- 'action', 'enableApplicationLayerAutomaticResponse_action' - Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
newEnableApplicationLayerAutomaticResponse ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'action'
  ResponseAction ->
  EnableApplicationLayerAutomaticResponse
newEnableApplicationLayerAutomaticResponse
  pResourceArn_
  pAction_ =
    EnableApplicationLayerAutomaticResponse'
      { resourceArn =
          pResourceArn_,
        action = pAction_
      }

-- | The ARN (Amazon Resource Name) of the protected resource.
enableApplicationLayerAutomaticResponse_resourceArn :: Lens.Lens' EnableApplicationLayerAutomaticResponse Prelude.Text
enableApplicationLayerAutomaticResponse_resourceArn = Lens.lens (\EnableApplicationLayerAutomaticResponse' {resourceArn} -> resourceArn) (\s@EnableApplicationLayerAutomaticResponse' {} a -> s {resourceArn = a} :: EnableApplicationLayerAutomaticResponse)

-- | Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
enableApplicationLayerAutomaticResponse_action :: Lens.Lens' EnableApplicationLayerAutomaticResponse ResponseAction
enableApplicationLayerAutomaticResponse_action = Lens.lens (\EnableApplicationLayerAutomaticResponse' {action} -> action) (\s@EnableApplicationLayerAutomaticResponse' {} a -> s {action = a} :: EnableApplicationLayerAutomaticResponse)

instance
  Core.AWSRequest
    EnableApplicationLayerAutomaticResponse
  where
  type
    AWSResponse
      EnableApplicationLayerAutomaticResponse =
      EnableApplicationLayerAutomaticResponseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableApplicationLayerAutomaticResponseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableApplicationLayerAutomaticResponse
  where
  hashWithSalt
    _salt
    EnableApplicationLayerAutomaticResponse' {..} =
      _salt `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    EnableApplicationLayerAutomaticResponse
  where
  rnf EnableApplicationLayerAutomaticResponse' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf action

instance
  Data.ToHeaders
    EnableApplicationLayerAutomaticResponse
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.EnableApplicationLayerAutomaticResponse" ::
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
    EnableApplicationLayerAutomaticResponse
  where
  toJSON EnableApplicationLayerAutomaticResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Action" Data..= action)
          ]
      )

instance
  Data.ToPath
    EnableApplicationLayerAutomaticResponse
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    EnableApplicationLayerAutomaticResponse
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableApplicationLayerAutomaticResponseResponse' smart constructor.
data EnableApplicationLayerAutomaticResponseResponse = EnableApplicationLayerAutomaticResponseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableApplicationLayerAutomaticResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableApplicationLayerAutomaticResponseResponse_httpStatus' - The response's http status code.
newEnableApplicationLayerAutomaticResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableApplicationLayerAutomaticResponseResponse
newEnableApplicationLayerAutomaticResponseResponse
  pHttpStatus_ =
    EnableApplicationLayerAutomaticResponseResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
enableApplicationLayerAutomaticResponseResponse_httpStatus :: Lens.Lens' EnableApplicationLayerAutomaticResponseResponse Prelude.Int
enableApplicationLayerAutomaticResponseResponse_httpStatus = Lens.lens (\EnableApplicationLayerAutomaticResponseResponse' {httpStatus} -> httpStatus) (\s@EnableApplicationLayerAutomaticResponseResponse' {} a -> s {httpStatus = a} :: EnableApplicationLayerAutomaticResponseResponse)

instance
  Prelude.NFData
    EnableApplicationLayerAutomaticResponseResponse
  where
  rnf
    EnableApplicationLayerAutomaticResponseResponse' {..} =
      Prelude.rnf httpStatus
