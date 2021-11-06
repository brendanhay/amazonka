{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Chime.Types.SipRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SipRule where

import Amazonka.Chime.Types.SipRuleTargetApplication
import Amazonka.Chime.Types.SipRuleTriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The SIP rule details, including name, triggers, and target applications.
-- An AWS account can have multiple SIP rules.
--
-- /See:/ 'newSipRule' smart constructor.
data SipRule = SipRule'
  { -- | Indicates whether the SIP rule is enabled or disabled. You must disable
    -- a rule before you can delete it.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | Target SIP media application and other details, such as priority and AWS
    -- Region, to be specified in the SIP rule. Only one SIP rule per AWS
    -- Region can be provided.
    targetApplications :: Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication),
    -- | The type of trigger assigned to the SIP rule in @TriggerValue@,
    -- currently @RequestUriHostname@ or @ToPhoneNumber@.
    triggerType :: Prelude.Maybe SipRuleTriggerType,
    -- | The time at which the SIP rule was last updated, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the SIP rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | If @TriggerType@ is @RequestUriHostname@, then the value can be the
    -- outbound host name of the Amazon Chime Voice Connector. If @TriggerType@
    -- is @ToPhoneNumber@, then the value can be a customer-owned phone number
    -- in E164 format. @SipRule@ is triggered when a SIP rule requests host
    -- name or @ToPhoneNumber@ matches in the incoming SIP request.
    triggerValue :: Prelude.Maybe Prelude.Text,
    -- | The time at which the SIP rule was created, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The SIP rule ID.
    sipRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'sipRule_disabled' - Indicates whether the SIP rule is enabled or disabled. You must disable
-- a rule before you can delete it.
--
-- 'targetApplications', 'sipRule_targetApplications' - Target SIP media application and other details, such as priority and AWS
-- Region, to be specified in the SIP rule. Only one SIP rule per AWS
-- Region can be provided.
--
-- 'triggerType', 'sipRule_triggerType' - The type of trigger assigned to the SIP rule in @TriggerValue@,
-- currently @RequestUriHostname@ or @ToPhoneNumber@.
--
-- 'updatedTimestamp', 'sipRule_updatedTimestamp' - The time at which the SIP rule was last updated, in ISO 8601 format.
--
-- 'name', 'sipRule_name' - The name of the SIP rule.
--
-- 'triggerValue', 'sipRule_triggerValue' - If @TriggerType@ is @RequestUriHostname@, then the value can be the
-- outbound host name of the Amazon Chime Voice Connector. If @TriggerType@
-- is @ToPhoneNumber@, then the value can be a customer-owned phone number
-- in E164 format. @SipRule@ is triggered when a SIP rule requests host
-- name or @ToPhoneNumber@ matches in the incoming SIP request.
--
-- 'createdTimestamp', 'sipRule_createdTimestamp' - The time at which the SIP rule was created, in ISO 8601 format.
--
-- 'sipRuleId', 'sipRule_sipRuleId' - The SIP rule ID.
newSipRule ::
  SipRule
newSipRule =
  SipRule'
    { disabled = Prelude.Nothing,
      targetApplications = Prelude.Nothing,
      triggerType = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      triggerValue = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      sipRuleId = Prelude.Nothing
    }

-- | Indicates whether the SIP rule is enabled or disabled. You must disable
-- a rule before you can delete it.
sipRule_disabled :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Bool)
sipRule_disabled = Lens.lens (\SipRule' {disabled} -> disabled) (\s@SipRule' {} a -> s {disabled = a} :: SipRule)

-- | Target SIP media application and other details, such as priority and AWS
-- Region, to be specified in the SIP rule. Only one SIP rule per AWS
-- Region can be provided.
sipRule_targetApplications :: Lens.Lens' SipRule (Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication))
sipRule_targetApplications = Lens.lens (\SipRule' {targetApplications} -> targetApplications) (\s@SipRule' {} a -> s {targetApplications = a} :: SipRule) Prelude.. Lens.mapping Lens.coerced

-- | The type of trigger assigned to the SIP rule in @TriggerValue@,
-- currently @RequestUriHostname@ or @ToPhoneNumber@.
sipRule_triggerType :: Lens.Lens' SipRule (Prelude.Maybe SipRuleTriggerType)
sipRule_triggerType = Lens.lens (\SipRule' {triggerType} -> triggerType) (\s@SipRule' {} a -> s {triggerType = a} :: SipRule)

-- | The time at which the SIP rule was last updated, in ISO 8601 format.
sipRule_updatedTimestamp :: Lens.Lens' SipRule (Prelude.Maybe Prelude.UTCTime)
sipRule_updatedTimestamp = Lens.lens (\SipRule' {updatedTimestamp} -> updatedTimestamp) (\s@SipRule' {} a -> s {updatedTimestamp = a} :: SipRule) Prelude.. Lens.mapping Core._Time

-- | The name of the SIP rule.
sipRule_name :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_name = Lens.lens (\SipRule' {name} -> name) (\s@SipRule' {} a -> s {name = a} :: SipRule)

-- | If @TriggerType@ is @RequestUriHostname@, then the value can be the
-- outbound host name of the Amazon Chime Voice Connector. If @TriggerType@
-- is @ToPhoneNumber@, then the value can be a customer-owned phone number
-- in E164 format. @SipRule@ is triggered when a SIP rule requests host
-- name or @ToPhoneNumber@ matches in the incoming SIP request.
sipRule_triggerValue :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_triggerValue = Lens.lens (\SipRule' {triggerValue} -> triggerValue) (\s@SipRule' {} a -> s {triggerValue = a} :: SipRule)

-- | The time at which the SIP rule was created, in ISO 8601 format.
sipRule_createdTimestamp :: Lens.Lens' SipRule (Prelude.Maybe Prelude.UTCTime)
sipRule_createdTimestamp = Lens.lens (\SipRule' {createdTimestamp} -> createdTimestamp) (\s@SipRule' {} a -> s {createdTimestamp = a} :: SipRule) Prelude.. Lens.mapping Core._Time

-- | The SIP rule ID.
sipRule_sipRuleId :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_sipRuleId = Lens.lens (\SipRule' {sipRuleId} -> sipRuleId) (\s@SipRule' {} a -> s {sipRuleId = a} :: SipRule)

instance Core.FromJSON SipRule where
  parseJSON =
    Core.withObject
      "SipRule"
      ( \x ->
          SipRule'
            Prelude.<$> (x Core..:? "Disabled")
            Prelude.<*> (x Core..:? "TargetApplications")
            Prelude.<*> (x Core..:? "TriggerType")
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "TriggerValue")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "SipRuleId")
      )

instance Prelude.Hashable SipRule

instance Prelude.NFData SipRule
