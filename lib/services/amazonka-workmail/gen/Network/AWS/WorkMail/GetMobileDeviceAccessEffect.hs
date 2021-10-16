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
-- Module      : Network.AWS.WorkMail.GetMobileDeviceAccessEffect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulates the effect of the mobile device access rules for the given
-- attributes of a sample access event. Use this method to test the effects
-- of the current set of mobile device access rules for the Amazon WorkMail
-- organization for a particular user\'s attributes.
module Network.AWS.WorkMail.GetMobileDeviceAccessEffect
  ( -- * Creating a Request
    GetMobileDeviceAccessEffect (..),
    newGetMobileDeviceAccessEffect,

    -- * Request Lenses
    getMobileDeviceAccessEffect_deviceModel,
    getMobileDeviceAccessEffect_deviceUserAgent,
    getMobileDeviceAccessEffect_deviceOperatingSystem,
    getMobileDeviceAccessEffect_deviceType,
    getMobileDeviceAccessEffect_organizationId,

    -- * Destructuring the Response
    GetMobileDeviceAccessEffectResponse (..),
    newGetMobileDeviceAccessEffectResponse,

    -- * Response Lenses
    getMobileDeviceAccessEffectResponse_matchedRules,
    getMobileDeviceAccessEffectResponse_effect,
    getMobileDeviceAccessEffectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newGetMobileDeviceAccessEffect' smart constructor.
data GetMobileDeviceAccessEffect = GetMobileDeviceAccessEffect'
  { -- | Device model the simulated user will report.
    deviceModel :: Prelude.Maybe Prelude.Text,
    -- | Device user agent the simulated user will report.
    deviceUserAgent :: Prelude.Maybe Prelude.Text,
    -- | Device operating system the simulated user will report.
    deviceOperatingSystem :: Prelude.Maybe Prelude.Text,
    -- | Device type the simulated user will report.
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon WorkMail organization to simulate the access effect for.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMobileDeviceAccessEffect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceModel', 'getMobileDeviceAccessEffect_deviceModel' - Device model the simulated user will report.
--
-- 'deviceUserAgent', 'getMobileDeviceAccessEffect_deviceUserAgent' - Device user agent the simulated user will report.
--
-- 'deviceOperatingSystem', 'getMobileDeviceAccessEffect_deviceOperatingSystem' - Device operating system the simulated user will report.
--
-- 'deviceType', 'getMobileDeviceAccessEffect_deviceType' - Device type the simulated user will report.
--
-- 'organizationId', 'getMobileDeviceAccessEffect_organizationId' - The Amazon WorkMail organization to simulate the access effect for.
newGetMobileDeviceAccessEffect ::
  -- | 'organizationId'
  Prelude.Text ->
  GetMobileDeviceAccessEffect
newGetMobileDeviceAccessEffect pOrganizationId_ =
  GetMobileDeviceAccessEffect'
    { deviceModel =
        Prelude.Nothing,
      deviceUserAgent = Prelude.Nothing,
      deviceOperatingSystem = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | Device model the simulated user will report.
getMobileDeviceAccessEffect_deviceModel :: Lens.Lens' GetMobileDeviceAccessEffect (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessEffect_deviceModel = Lens.lens (\GetMobileDeviceAccessEffect' {deviceModel} -> deviceModel) (\s@GetMobileDeviceAccessEffect' {} a -> s {deviceModel = a} :: GetMobileDeviceAccessEffect)

-- | Device user agent the simulated user will report.
getMobileDeviceAccessEffect_deviceUserAgent :: Lens.Lens' GetMobileDeviceAccessEffect (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessEffect_deviceUserAgent = Lens.lens (\GetMobileDeviceAccessEffect' {deviceUserAgent} -> deviceUserAgent) (\s@GetMobileDeviceAccessEffect' {} a -> s {deviceUserAgent = a} :: GetMobileDeviceAccessEffect)

-- | Device operating system the simulated user will report.
getMobileDeviceAccessEffect_deviceOperatingSystem :: Lens.Lens' GetMobileDeviceAccessEffect (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessEffect_deviceOperatingSystem = Lens.lens (\GetMobileDeviceAccessEffect' {deviceOperatingSystem} -> deviceOperatingSystem) (\s@GetMobileDeviceAccessEffect' {} a -> s {deviceOperatingSystem = a} :: GetMobileDeviceAccessEffect)

-- | Device type the simulated user will report.
getMobileDeviceAccessEffect_deviceType :: Lens.Lens' GetMobileDeviceAccessEffect (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessEffect_deviceType = Lens.lens (\GetMobileDeviceAccessEffect' {deviceType} -> deviceType) (\s@GetMobileDeviceAccessEffect' {} a -> s {deviceType = a} :: GetMobileDeviceAccessEffect)

-- | The Amazon WorkMail organization to simulate the access effect for.
getMobileDeviceAccessEffect_organizationId :: Lens.Lens' GetMobileDeviceAccessEffect Prelude.Text
getMobileDeviceAccessEffect_organizationId = Lens.lens (\GetMobileDeviceAccessEffect' {organizationId} -> organizationId) (\s@GetMobileDeviceAccessEffect' {} a -> s {organizationId = a} :: GetMobileDeviceAccessEffect)

instance Core.AWSRequest GetMobileDeviceAccessEffect where
  type
    AWSResponse GetMobileDeviceAccessEffect =
      GetMobileDeviceAccessEffectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMobileDeviceAccessEffectResponse'
            Prelude.<$> (x Core..?> "MatchedRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Effect")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMobileDeviceAccessEffect

instance Prelude.NFData GetMobileDeviceAccessEffect

instance Core.ToHeaders GetMobileDeviceAccessEffect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.GetMobileDeviceAccessEffect" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMobileDeviceAccessEffect where
  toJSON GetMobileDeviceAccessEffect' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeviceModel" Core..=) Prelude.<$> deviceModel,
            ("DeviceUserAgent" Core..=)
              Prelude.<$> deviceUserAgent,
            ("DeviceOperatingSystem" Core..=)
              Prelude.<$> deviceOperatingSystem,
            ("DeviceType" Core..=) Prelude.<$> deviceType,
            Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath GetMobileDeviceAccessEffect where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMobileDeviceAccessEffect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMobileDeviceAccessEffectResponse' smart constructor.
data GetMobileDeviceAccessEffectResponse = GetMobileDeviceAccessEffectResponse'
  { -- | A list of the rules which matched the simulated user input and produced
    -- the effect.
    matchedRules :: Prelude.Maybe [MobileDeviceAccessMatchedRule],
    -- | The effect of the simulated access, @ALLOW@ or @DENY@, after evaluating
    -- mobile device access rules in the Amazon WorkMail organization for the
    -- simulated user parameters.
    effect :: Prelude.Maybe MobileDeviceAccessRuleEffect,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMobileDeviceAccessEffectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchedRules', 'getMobileDeviceAccessEffectResponse_matchedRules' - A list of the rules which matched the simulated user input and produced
-- the effect.
--
-- 'effect', 'getMobileDeviceAccessEffectResponse_effect' - The effect of the simulated access, @ALLOW@ or @DENY@, after evaluating
-- mobile device access rules in the Amazon WorkMail organization for the
-- simulated user parameters.
--
-- 'httpStatus', 'getMobileDeviceAccessEffectResponse_httpStatus' - The response's http status code.
newGetMobileDeviceAccessEffectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMobileDeviceAccessEffectResponse
newGetMobileDeviceAccessEffectResponse pHttpStatus_ =
  GetMobileDeviceAccessEffectResponse'
    { matchedRules =
        Prelude.Nothing,
      effect = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the rules which matched the simulated user input and produced
-- the effect.
getMobileDeviceAccessEffectResponse_matchedRules :: Lens.Lens' GetMobileDeviceAccessEffectResponse (Prelude.Maybe [MobileDeviceAccessMatchedRule])
getMobileDeviceAccessEffectResponse_matchedRules = Lens.lens (\GetMobileDeviceAccessEffectResponse' {matchedRules} -> matchedRules) (\s@GetMobileDeviceAccessEffectResponse' {} a -> s {matchedRules = a} :: GetMobileDeviceAccessEffectResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The effect of the simulated access, @ALLOW@ or @DENY@, after evaluating
-- mobile device access rules in the Amazon WorkMail organization for the
-- simulated user parameters.
getMobileDeviceAccessEffectResponse_effect :: Lens.Lens' GetMobileDeviceAccessEffectResponse (Prelude.Maybe MobileDeviceAccessRuleEffect)
getMobileDeviceAccessEffectResponse_effect = Lens.lens (\GetMobileDeviceAccessEffectResponse' {effect} -> effect) (\s@GetMobileDeviceAccessEffectResponse' {} a -> s {effect = a} :: GetMobileDeviceAccessEffectResponse)

-- | The response's http status code.
getMobileDeviceAccessEffectResponse_httpStatus :: Lens.Lens' GetMobileDeviceAccessEffectResponse Prelude.Int
getMobileDeviceAccessEffectResponse_httpStatus = Lens.lens (\GetMobileDeviceAccessEffectResponse' {httpStatus} -> httpStatus) (\s@GetMobileDeviceAccessEffectResponse' {} a -> s {httpStatus = a} :: GetMobileDeviceAccessEffectResponse)

instance
  Prelude.NFData
    GetMobileDeviceAccessEffectResponse
