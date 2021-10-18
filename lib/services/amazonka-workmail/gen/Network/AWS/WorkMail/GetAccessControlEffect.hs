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
-- Module      : Network.AWS.WorkMail.GetAccessControlEffect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the effects of an organization\'s access control rules as they
-- apply to a specified IPv4 address, access protocol action, or user ID.
module Network.AWS.WorkMail.GetAccessControlEffect
  ( -- * Creating a Request
    GetAccessControlEffect (..),
    newGetAccessControlEffect,

    -- * Request Lenses
    getAccessControlEffect_organizationId,
    getAccessControlEffect_ipAddress,
    getAccessControlEffect_action,
    getAccessControlEffect_userId,

    -- * Destructuring the Response
    GetAccessControlEffectResponse (..),
    newGetAccessControlEffectResponse,

    -- * Response Lenses
    getAccessControlEffectResponse_matchedRules,
    getAccessControlEffectResponse_effect,
    getAccessControlEffectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newGetAccessControlEffect' smart constructor.
data GetAccessControlEffect = GetAccessControlEffect'
  { -- | The identifier for the organization.
    organizationId :: Prelude.Text,
    -- | The IPv4 address.
    ipAddress :: Prelude.Text,
    -- | The access protocol action. Valid values include @ActiveSync@,
    -- @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@, and @WebMail@.
    action :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessControlEffect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'getAccessControlEffect_organizationId' - The identifier for the organization.
--
-- 'ipAddress', 'getAccessControlEffect_ipAddress' - The IPv4 address.
--
-- 'action', 'getAccessControlEffect_action' - The access protocol action. Valid values include @ActiveSync@,
-- @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@, and @WebMail@.
--
-- 'userId', 'getAccessControlEffect_userId' - The user ID.
newGetAccessControlEffect ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'action'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  GetAccessControlEffect
newGetAccessControlEffect
  pOrganizationId_
  pIpAddress_
  pAction_
  pUserId_ =
    GetAccessControlEffect'
      { organizationId =
          pOrganizationId_,
        ipAddress = pIpAddress_,
        action = pAction_,
        userId = pUserId_
      }

-- | The identifier for the organization.
getAccessControlEffect_organizationId :: Lens.Lens' GetAccessControlEffect Prelude.Text
getAccessControlEffect_organizationId = Lens.lens (\GetAccessControlEffect' {organizationId} -> organizationId) (\s@GetAccessControlEffect' {} a -> s {organizationId = a} :: GetAccessControlEffect)

-- | The IPv4 address.
getAccessControlEffect_ipAddress :: Lens.Lens' GetAccessControlEffect Prelude.Text
getAccessControlEffect_ipAddress = Lens.lens (\GetAccessControlEffect' {ipAddress} -> ipAddress) (\s@GetAccessControlEffect' {} a -> s {ipAddress = a} :: GetAccessControlEffect)

-- | The access protocol action. Valid values include @ActiveSync@,
-- @AutoDiscover@, @EWS@, @IMAP@, @SMTP@, @WindowsOutlook@, and @WebMail@.
getAccessControlEffect_action :: Lens.Lens' GetAccessControlEffect Prelude.Text
getAccessControlEffect_action = Lens.lens (\GetAccessControlEffect' {action} -> action) (\s@GetAccessControlEffect' {} a -> s {action = a} :: GetAccessControlEffect)

-- | The user ID.
getAccessControlEffect_userId :: Lens.Lens' GetAccessControlEffect Prelude.Text
getAccessControlEffect_userId = Lens.lens (\GetAccessControlEffect' {userId} -> userId) (\s@GetAccessControlEffect' {} a -> s {userId = a} :: GetAccessControlEffect)

instance Core.AWSRequest GetAccessControlEffect where
  type
    AWSResponse GetAccessControlEffect =
      GetAccessControlEffectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccessControlEffectResponse'
            Prelude.<$> (x Core..?> "MatchedRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Effect")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessControlEffect

instance Prelude.NFData GetAccessControlEffect

instance Core.ToHeaders GetAccessControlEffect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.GetAccessControlEffect" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAccessControlEffect where
  toJSON GetAccessControlEffect' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("IpAddress" Core..= ipAddress),
            Prelude.Just ("Action" Core..= action),
            Prelude.Just ("UserId" Core..= userId)
          ]
      )

instance Core.ToPath GetAccessControlEffect where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAccessControlEffect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccessControlEffectResponse' smart constructor.
data GetAccessControlEffectResponse = GetAccessControlEffectResponse'
  { -- | The rules that match the given parameters, resulting in an effect.
    matchedRules :: Prelude.Maybe [Prelude.Text],
    -- | The rule effect.
    effect :: Prelude.Maybe AccessControlRuleEffect,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessControlEffectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchedRules', 'getAccessControlEffectResponse_matchedRules' - The rules that match the given parameters, resulting in an effect.
--
-- 'effect', 'getAccessControlEffectResponse_effect' - The rule effect.
--
-- 'httpStatus', 'getAccessControlEffectResponse_httpStatus' - The response's http status code.
newGetAccessControlEffectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessControlEffectResponse
newGetAccessControlEffectResponse pHttpStatus_ =
  GetAccessControlEffectResponse'
    { matchedRules =
        Prelude.Nothing,
      effect = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The rules that match the given parameters, resulting in an effect.
getAccessControlEffectResponse_matchedRules :: Lens.Lens' GetAccessControlEffectResponse (Prelude.Maybe [Prelude.Text])
getAccessControlEffectResponse_matchedRules = Lens.lens (\GetAccessControlEffectResponse' {matchedRules} -> matchedRules) (\s@GetAccessControlEffectResponse' {} a -> s {matchedRules = a} :: GetAccessControlEffectResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The rule effect.
getAccessControlEffectResponse_effect :: Lens.Lens' GetAccessControlEffectResponse (Prelude.Maybe AccessControlRuleEffect)
getAccessControlEffectResponse_effect = Lens.lens (\GetAccessControlEffectResponse' {effect} -> effect) (\s@GetAccessControlEffectResponse' {} a -> s {effect = a} :: GetAccessControlEffectResponse)

-- | The response's http status code.
getAccessControlEffectResponse_httpStatus :: Lens.Lens' GetAccessControlEffectResponse Prelude.Int
getAccessControlEffectResponse_httpStatus = Lens.lens (\GetAccessControlEffectResponse' {httpStatus} -> httpStatus) (\s@GetAccessControlEffectResponse' {} a -> s {httpStatus = a} :: GetAccessControlEffectResponse)

instance
  Prelude.NFData
    GetAccessControlEffectResponse
