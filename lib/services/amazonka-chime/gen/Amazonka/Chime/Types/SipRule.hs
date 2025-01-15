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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SipRule where

import Amazonka.Chime.Types.SipRuleTargetApplication
import Amazonka.Chime.Types.SipRuleTriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The SIP rule details, including name, triggers, and target applications.
-- An AWS account can have multiple SIP rules.
--
-- /See:/ 'newSipRule' smart constructor.
data SipRule = SipRule'
  { -- | The time at which the SIP rule was created, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether the SIP rule is enabled or disabled. You must disable
    -- a rule before you can delete it.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the SIP rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The SIP rule ID.
    sipRuleId :: Prelude.Maybe Prelude.Text,
    -- | Target SIP media application and other details, such as priority and AWS
    -- Region, to be specified in the SIP rule. Only one SIP rule per AWS
    -- Region can be provided.
    targetApplications :: Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication),
    -- | The type of trigger assigned to the SIP rule in @TriggerValue@,
    -- currently @RequestUriHostname@ or @ToPhoneNumber@.
    triggerType :: Prelude.Maybe SipRuleTriggerType,
    -- | If @TriggerType@ is @RequestUriHostname@, then the value can be the
    -- outbound host name of the Amazon Chime Voice Connector. If @TriggerType@
    -- is @ToPhoneNumber@, then the value can be a customer-owned phone number
    -- in E164 format. @SipRule@ is triggered when a SIP rule requests host
    -- name or @ToPhoneNumber@ matches in the incoming SIP request.
    triggerValue :: Prelude.Maybe Prelude.Text,
    -- | The time at which the SIP rule was last updated, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
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
-- 'createdTimestamp', 'sipRule_createdTimestamp' - The time at which the SIP rule was created, in ISO 8601 format.
--
-- 'disabled', 'sipRule_disabled' - Indicates whether the SIP rule is enabled or disabled. You must disable
-- a rule before you can delete it.
--
-- 'name', 'sipRule_name' - The name of the SIP rule.
--
-- 'sipRuleId', 'sipRule_sipRuleId' - The SIP rule ID.
--
-- 'targetApplications', 'sipRule_targetApplications' - Target SIP media application and other details, such as priority and AWS
-- Region, to be specified in the SIP rule. Only one SIP rule per AWS
-- Region can be provided.
--
-- 'triggerType', 'sipRule_triggerType' - The type of trigger assigned to the SIP rule in @TriggerValue@,
-- currently @RequestUriHostname@ or @ToPhoneNumber@.
--
-- 'triggerValue', 'sipRule_triggerValue' - If @TriggerType@ is @RequestUriHostname@, then the value can be the
-- outbound host name of the Amazon Chime Voice Connector. If @TriggerType@
-- is @ToPhoneNumber@, then the value can be a customer-owned phone number
-- in E164 format. @SipRule@ is triggered when a SIP rule requests host
-- name or @ToPhoneNumber@ matches in the incoming SIP request.
--
-- 'updatedTimestamp', 'sipRule_updatedTimestamp' - The time at which the SIP rule was last updated, in ISO 8601 format.
newSipRule ::
  SipRule
newSipRule =
  SipRule'
    { createdTimestamp = Prelude.Nothing,
      disabled = Prelude.Nothing,
      name = Prelude.Nothing,
      sipRuleId = Prelude.Nothing,
      targetApplications = Prelude.Nothing,
      triggerType = Prelude.Nothing,
      triggerValue = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The time at which the SIP rule was created, in ISO 8601 format.
sipRule_createdTimestamp :: Lens.Lens' SipRule (Prelude.Maybe Prelude.UTCTime)
sipRule_createdTimestamp = Lens.lens (\SipRule' {createdTimestamp} -> createdTimestamp) (\s@SipRule' {} a -> s {createdTimestamp = a} :: SipRule) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the SIP rule is enabled or disabled. You must disable
-- a rule before you can delete it.
sipRule_disabled :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Bool)
sipRule_disabled = Lens.lens (\SipRule' {disabled} -> disabled) (\s@SipRule' {} a -> s {disabled = a} :: SipRule)

-- | The name of the SIP rule.
sipRule_name :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_name = Lens.lens (\SipRule' {name} -> name) (\s@SipRule' {} a -> s {name = a} :: SipRule)

-- | The SIP rule ID.
sipRule_sipRuleId :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_sipRuleId = Lens.lens (\SipRule' {sipRuleId} -> sipRuleId) (\s@SipRule' {} a -> s {sipRuleId = a} :: SipRule)

-- | Target SIP media application and other details, such as priority and AWS
-- Region, to be specified in the SIP rule. Only one SIP rule per AWS
-- Region can be provided.
sipRule_targetApplications :: Lens.Lens' SipRule (Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication))
sipRule_targetApplications = Lens.lens (\SipRule' {targetApplications} -> targetApplications) (\s@SipRule' {} a -> s {targetApplications = a} :: SipRule) Prelude.. Lens.mapping Lens.coerced

-- | The type of trigger assigned to the SIP rule in @TriggerValue@,
-- currently @RequestUriHostname@ or @ToPhoneNumber@.
sipRule_triggerType :: Lens.Lens' SipRule (Prelude.Maybe SipRuleTriggerType)
sipRule_triggerType = Lens.lens (\SipRule' {triggerType} -> triggerType) (\s@SipRule' {} a -> s {triggerType = a} :: SipRule)

-- | If @TriggerType@ is @RequestUriHostname@, then the value can be the
-- outbound host name of the Amazon Chime Voice Connector. If @TriggerType@
-- is @ToPhoneNumber@, then the value can be a customer-owned phone number
-- in E164 format. @SipRule@ is triggered when a SIP rule requests host
-- name or @ToPhoneNumber@ matches in the incoming SIP request.
sipRule_triggerValue :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_triggerValue = Lens.lens (\SipRule' {triggerValue} -> triggerValue) (\s@SipRule' {} a -> s {triggerValue = a} :: SipRule)

-- | The time at which the SIP rule was last updated, in ISO 8601 format.
sipRule_updatedTimestamp :: Lens.Lens' SipRule (Prelude.Maybe Prelude.UTCTime)
sipRule_updatedTimestamp = Lens.lens (\SipRule' {updatedTimestamp} -> updatedTimestamp) (\s@SipRule' {} a -> s {updatedTimestamp = a} :: SipRule) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SipRule where
  parseJSON =
    Data.withObject
      "SipRule"
      ( \x ->
          SipRule'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Disabled")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SipRuleId")
            Prelude.<*> (x Data..:? "TargetApplications")
            Prelude.<*> (x Data..:? "TriggerType")
            Prelude.<*> (x Data..:? "TriggerValue")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable SipRule where
  hashWithSalt _salt SipRule' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sipRuleId
      `Prelude.hashWithSalt` targetApplications
      `Prelude.hashWithSalt` triggerType
      `Prelude.hashWithSalt` triggerValue
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData SipRule where
  rnf SipRule' {..} =
    Prelude.rnf createdTimestamp `Prelude.seq`
      Prelude.rnf disabled `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf sipRuleId `Prelude.seq`
            Prelude.rnf targetApplications `Prelude.seq`
              Prelude.rnf triggerType `Prelude.seq`
                Prelude.rnf triggerValue `Prelude.seq`
                  Prelude.rnf updatedTimestamp
