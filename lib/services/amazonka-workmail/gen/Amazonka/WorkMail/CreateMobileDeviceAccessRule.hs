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
-- Module      : Amazonka.WorkMail.CreateMobileDeviceAccessRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new mobile device access rule for the specified Amazon
-- WorkMail organization.
module Amazonka.WorkMail.CreateMobileDeviceAccessRule
  ( -- * Creating a Request
    CreateMobileDeviceAccessRule (..),
    newCreateMobileDeviceAccessRule,

    -- * Request Lenses
    createMobileDeviceAccessRule_clientToken,
    createMobileDeviceAccessRule_deviceUserAgents,
    createMobileDeviceAccessRule_deviceTypes,
    createMobileDeviceAccessRule_notDeviceTypes,
    createMobileDeviceAccessRule_notDeviceOperatingSystems,
    createMobileDeviceAccessRule_deviceModels,
    createMobileDeviceAccessRule_deviceOperatingSystems,
    createMobileDeviceAccessRule_description,
    createMobileDeviceAccessRule_notDeviceUserAgents,
    createMobileDeviceAccessRule_notDeviceModels,
    createMobileDeviceAccessRule_organizationId,
    createMobileDeviceAccessRule_name,
    createMobileDeviceAccessRule_effect,

    -- * Destructuring the Response
    CreateMobileDeviceAccessRuleResponse (..),
    newCreateMobileDeviceAccessRuleResponse,

    -- * Response Lenses
    createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId,
    createMobileDeviceAccessRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newCreateMobileDeviceAccessRule' smart constructor.
data CreateMobileDeviceAccessRule = CreateMobileDeviceAccessRule'
  { -- | The idempotency token for the client request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Device user agents that the rule will match.
    deviceUserAgents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device types that the rule will match.
    deviceTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device types that the rule __will not__ match. All other device types
    -- will match.
    notDeviceTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device operating systems that the rule __will not__ match. All other
    -- device operating systems will match.
    notDeviceOperatingSystems :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device models that the rule will match.
    deviceModels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device operating systems that the rule will match.
    deviceOperatingSystems :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Device user agents that the rule __will not__ match. All other device
    -- user agents will match.
    notDeviceUserAgents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device models that the rule __will not__ match. All other device models
    -- will match.
    notDeviceModels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon WorkMail organization under which the rule will be created.
    organizationId :: Prelude.Text,
    -- | The rule name.
    name :: Prelude.Text,
    -- | The effect of the rule when it matches. Allowed values are @ALLOW@ or
    -- @DENY@.
    effect :: MobileDeviceAccessRuleEffect
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMobileDeviceAccessRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createMobileDeviceAccessRule_clientToken' - The idempotency token for the client request.
--
-- 'deviceUserAgents', 'createMobileDeviceAccessRule_deviceUserAgents' - Device user agents that the rule will match.
--
-- 'deviceTypes', 'createMobileDeviceAccessRule_deviceTypes' - Device types that the rule will match.
--
-- 'notDeviceTypes', 'createMobileDeviceAccessRule_notDeviceTypes' - Device types that the rule __will not__ match. All other device types
-- will match.
--
-- 'notDeviceOperatingSystems', 'createMobileDeviceAccessRule_notDeviceOperatingSystems' - Device operating systems that the rule __will not__ match. All other
-- device operating systems will match.
--
-- 'deviceModels', 'createMobileDeviceAccessRule_deviceModels' - Device models that the rule will match.
--
-- 'deviceOperatingSystems', 'createMobileDeviceAccessRule_deviceOperatingSystems' - Device operating systems that the rule will match.
--
-- 'description', 'createMobileDeviceAccessRule_description' - The rule description.
--
-- 'notDeviceUserAgents', 'createMobileDeviceAccessRule_notDeviceUserAgents' - Device user agents that the rule __will not__ match. All other device
-- user agents will match.
--
-- 'notDeviceModels', 'createMobileDeviceAccessRule_notDeviceModels' - Device models that the rule __will not__ match. All other device models
-- will match.
--
-- 'organizationId', 'createMobileDeviceAccessRule_organizationId' - The Amazon WorkMail organization under which the rule will be created.
--
-- 'name', 'createMobileDeviceAccessRule_name' - The rule name.
--
-- 'effect', 'createMobileDeviceAccessRule_effect' - The effect of the rule when it matches. Allowed values are @ALLOW@ or
-- @DENY@.
newCreateMobileDeviceAccessRule ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'effect'
  MobileDeviceAccessRuleEffect ->
  CreateMobileDeviceAccessRule
newCreateMobileDeviceAccessRule
  pOrganizationId_
  pName_
  pEffect_ =
    CreateMobileDeviceAccessRule'
      { clientToken =
          Prelude.Nothing,
        deviceUserAgents = Prelude.Nothing,
        deviceTypes = Prelude.Nothing,
        notDeviceTypes = Prelude.Nothing,
        notDeviceOperatingSystems = Prelude.Nothing,
        deviceModels = Prelude.Nothing,
        deviceOperatingSystems = Prelude.Nothing,
        description = Prelude.Nothing,
        notDeviceUserAgents = Prelude.Nothing,
        notDeviceModels = Prelude.Nothing,
        organizationId = pOrganizationId_,
        name = pName_,
        effect = pEffect_
      }

-- | The idempotency token for the client request.
createMobileDeviceAccessRule_clientToken :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
createMobileDeviceAccessRule_clientToken = Lens.lens (\CreateMobileDeviceAccessRule' {clientToken} -> clientToken) (\s@CreateMobileDeviceAccessRule' {} a -> s {clientToken = a} :: CreateMobileDeviceAccessRule)

-- | Device user agents that the rule will match.
createMobileDeviceAccessRule_deviceUserAgents :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_deviceUserAgents = Lens.lens (\CreateMobileDeviceAccessRule' {deviceUserAgents} -> deviceUserAgents) (\s@CreateMobileDeviceAccessRule' {} a -> s {deviceUserAgents = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device types that the rule will match.
createMobileDeviceAccessRule_deviceTypes :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_deviceTypes = Lens.lens (\CreateMobileDeviceAccessRule' {deviceTypes} -> deviceTypes) (\s@CreateMobileDeviceAccessRule' {} a -> s {deviceTypes = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device types that the rule __will not__ match. All other device types
-- will match.
createMobileDeviceAccessRule_notDeviceTypes :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_notDeviceTypes = Lens.lens (\CreateMobileDeviceAccessRule' {notDeviceTypes} -> notDeviceTypes) (\s@CreateMobileDeviceAccessRule' {} a -> s {notDeviceTypes = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device operating systems that the rule __will not__ match. All other
-- device operating systems will match.
createMobileDeviceAccessRule_notDeviceOperatingSystems :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_notDeviceOperatingSystems = Lens.lens (\CreateMobileDeviceAccessRule' {notDeviceOperatingSystems} -> notDeviceOperatingSystems) (\s@CreateMobileDeviceAccessRule' {} a -> s {notDeviceOperatingSystems = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device models that the rule will match.
createMobileDeviceAccessRule_deviceModels :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_deviceModels = Lens.lens (\CreateMobileDeviceAccessRule' {deviceModels} -> deviceModels) (\s@CreateMobileDeviceAccessRule' {} a -> s {deviceModels = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device operating systems that the rule will match.
createMobileDeviceAccessRule_deviceOperatingSystems :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_deviceOperatingSystems = Lens.lens (\CreateMobileDeviceAccessRule' {deviceOperatingSystems} -> deviceOperatingSystems) (\s@CreateMobileDeviceAccessRule' {} a -> s {deviceOperatingSystems = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule description.
createMobileDeviceAccessRule_description :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
createMobileDeviceAccessRule_description = Lens.lens (\CreateMobileDeviceAccessRule' {description} -> description) (\s@CreateMobileDeviceAccessRule' {} a -> s {description = a} :: CreateMobileDeviceAccessRule)

-- | Device user agents that the rule __will not__ match. All other device
-- user agents will match.
createMobileDeviceAccessRule_notDeviceUserAgents :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_notDeviceUserAgents = Lens.lens (\CreateMobileDeviceAccessRule' {notDeviceUserAgents} -> notDeviceUserAgents) (\s@CreateMobileDeviceAccessRule' {} a -> s {notDeviceUserAgents = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device models that the rule __will not__ match. All other device models
-- will match.
createMobileDeviceAccessRule_notDeviceModels :: Lens.Lens' CreateMobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMobileDeviceAccessRule_notDeviceModels = Lens.lens (\CreateMobileDeviceAccessRule' {notDeviceModels} -> notDeviceModels) (\s@CreateMobileDeviceAccessRule' {} a -> s {notDeviceModels = a} :: CreateMobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon WorkMail organization under which the rule will be created.
createMobileDeviceAccessRule_organizationId :: Lens.Lens' CreateMobileDeviceAccessRule Prelude.Text
createMobileDeviceAccessRule_organizationId = Lens.lens (\CreateMobileDeviceAccessRule' {organizationId} -> organizationId) (\s@CreateMobileDeviceAccessRule' {} a -> s {organizationId = a} :: CreateMobileDeviceAccessRule)

-- | The rule name.
createMobileDeviceAccessRule_name :: Lens.Lens' CreateMobileDeviceAccessRule Prelude.Text
createMobileDeviceAccessRule_name = Lens.lens (\CreateMobileDeviceAccessRule' {name} -> name) (\s@CreateMobileDeviceAccessRule' {} a -> s {name = a} :: CreateMobileDeviceAccessRule)

-- | The effect of the rule when it matches. Allowed values are @ALLOW@ or
-- @DENY@.
createMobileDeviceAccessRule_effect :: Lens.Lens' CreateMobileDeviceAccessRule MobileDeviceAccessRuleEffect
createMobileDeviceAccessRule_effect = Lens.lens (\CreateMobileDeviceAccessRule' {effect} -> effect) (\s@CreateMobileDeviceAccessRule' {} a -> s {effect = a} :: CreateMobileDeviceAccessRule)

instance Core.AWSRequest CreateMobileDeviceAccessRule where
  type
    AWSResponse CreateMobileDeviceAccessRule =
      CreateMobileDeviceAccessRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMobileDeviceAccessRuleResponse'
            Prelude.<$> (x Core..?> "MobileDeviceAccessRuleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMobileDeviceAccessRule
  where
  hashWithSalt _salt CreateMobileDeviceAccessRule' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` deviceUserAgents
      `Prelude.hashWithSalt` deviceTypes
      `Prelude.hashWithSalt` notDeviceTypes
      `Prelude.hashWithSalt` notDeviceOperatingSystems
      `Prelude.hashWithSalt` deviceModels
      `Prelude.hashWithSalt` deviceOperatingSystems
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notDeviceUserAgents
      `Prelude.hashWithSalt` notDeviceModels
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` effect

instance Prelude.NFData CreateMobileDeviceAccessRule where
  rnf CreateMobileDeviceAccessRule' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf deviceUserAgents
      `Prelude.seq` Prelude.rnf deviceTypes
      `Prelude.seq` Prelude.rnf notDeviceTypes
      `Prelude.seq` Prelude.rnf notDeviceOperatingSystems
      `Prelude.seq` Prelude.rnf deviceModels
      `Prelude.seq` Prelude.rnf deviceOperatingSystems
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notDeviceUserAgents
      `Prelude.seq` Prelude.rnf notDeviceModels
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf effect

instance Core.ToHeaders CreateMobileDeviceAccessRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.CreateMobileDeviceAccessRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMobileDeviceAccessRule where
  toJSON CreateMobileDeviceAccessRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("DeviceUserAgents" Core..=)
              Prelude.<$> deviceUserAgents,
            ("DeviceTypes" Core..=) Prelude.<$> deviceTypes,
            ("NotDeviceTypes" Core..=)
              Prelude.<$> notDeviceTypes,
            ("NotDeviceOperatingSystems" Core..=)
              Prelude.<$> notDeviceOperatingSystems,
            ("DeviceModels" Core..=) Prelude.<$> deviceModels,
            ("DeviceOperatingSystems" Core..=)
              Prelude.<$> deviceOperatingSystems,
            ("Description" Core..=) Prelude.<$> description,
            ("NotDeviceUserAgents" Core..=)
              Prelude.<$> notDeviceUserAgents,
            ("NotDeviceModels" Core..=)
              Prelude.<$> notDeviceModels,
            Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Effect" Core..= effect)
          ]
      )

instance Core.ToPath CreateMobileDeviceAccessRule where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateMobileDeviceAccessRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMobileDeviceAccessRuleResponse' smart constructor.
data CreateMobileDeviceAccessRuleResponse = CreateMobileDeviceAccessRuleResponse'
  { -- | The identifier for the newly created mobile device access rule.
    mobileDeviceAccessRuleId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMobileDeviceAccessRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mobileDeviceAccessRuleId', 'createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId' - The identifier for the newly created mobile device access rule.
--
-- 'httpStatus', 'createMobileDeviceAccessRuleResponse_httpStatus' - The response's http status code.
newCreateMobileDeviceAccessRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMobileDeviceAccessRuleResponse
newCreateMobileDeviceAccessRuleResponse pHttpStatus_ =
  CreateMobileDeviceAccessRuleResponse'
    { mobileDeviceAccessRuleId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the newly created mobile device access rule.
createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId :: Lens.Lens' CreateMobileDeviceAccessRuleResponse (Prelude.Maybe Prelude.Text)
createMobileDeviceAccessRuleResponse_mobileDeviceAccessRuleId = Lens.lens (\CreateMobileDeviceAccessRuleResponse' {mobileDeviceAccessRuleId} -> mobileDeviceAccessRuleId) (\s@CreateMobileDeviceAccessRuleResponse' {} a -> s {mobileDeviceAccessRuleId = a} :: CreateMobileDeviceAccessRuleResponse)

-- | The response's http status code.
createMobileDeviceAccessRuleResponse_httpStatus :: Lens.Lens' CreateMobileDeviceAccessRuleResponse Prelude.Int
createMobileDeviceAccessRuleResponse_httpStatus = Lens.lens (\CreateMobileDeviceAccessRuleResponse' {httpStatus} -> httpStatus) (\s@CreateMobileDeviceAccessRuleResponse' {} a -> s {httpStatus = a} :: CreateMobileDeviceAccessRuleResponse)

instance
  Prelude.NFData
    CreateMobileDeviceAccessRuleResponse
  where
  rnf CreateMobileDeviceAccessRuleResponse' {..} =
    Prelude.rnf mobileDeviceAccessRuleId
      `Prelude.seq` Prelude.rnf httpStatus
