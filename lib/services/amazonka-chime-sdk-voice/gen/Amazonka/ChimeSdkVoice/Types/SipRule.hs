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
-- Module      : Amazonka.ChimeSdkVoice.Types.SipRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SipRule where

import Amazonka.ChimeSdkVoice.Types.SipRuleTargetApplication
import Amazonka.ChimeSdkVoice.Types.SipRuleTriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newSipRule' smart constructor.
data SipRule = SipRule'
  { createdTimestamp :: Prelude.Maybe Data.ISO8601,
    disabled :: Prelude.Maybe Prelude.Bool,
    name :: Prelude.Maybe Prelude.Text,
    sipRuleId :: Prelude.Maybe Prelude.Text,
    targetApplications :: Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication),
    triggerType :: Prelude.Maybe SipRuleTriggerType,
    triggerValue :: Prelude.Maybe Prelude.Text,
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
-- 'createdTimestamp', 'sipRule_createdTimestamp' - Undocumented member.
--
-- 'disabled', 'sipRule_disabled' - Undocumented member.
--
-- 'name', 'sipRule_name' - Undocumented member.
--
-- 'sipRuleId', 'sipRule_sipRuleId' - Undocumented member.
--
-- 'targetApplications', 'sipRule_targetApplications' - Undocumented member.
--
-- 'triggerType', 'sipRule_triggerType' - Undocumented member.
--
-- 'triggerValue', 'sipRule_triggerValue' - Undocumented member.
--
-- 'updatedTimestamp', 'sipRule_updatedTimestamp' - Undocumented member.
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

-- | Undocumented member.
sipRule_createdTimestamp :: Lens.Lens' SipRule (Prelude.Maybe Prelude.UTCTime)
sipRule_createdTimestamp = Lens.lens (\SipRule' {createdTimestamp} -> createdTimestamp) (\s@SipRule' {} a -> s {createdTimestamp = a} :: SipRule) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
sipRule_disabled :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Bool)
sipRule_disabled = Lens.lens (\SipRule' {disabled} -> disabled) (\s@SipRule' {} a -> s {disabled = a} :: SipRule)

-- | Undocumented member.
sipRule_name :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_name = Lens.lens (\SipRule' {name} -> name) (\s@SipRule' {} a -> s {name = a} :: SipRule)

-- | Undocumented member.
sipRule_sipRuleId :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_sipRuleId = Lens.lens (\SipRule' {sipRuleId} -> sipRuleId) (\s@SipRule' {} a -> s {sipRuleId = a} :: SipRule)

-- | Undocumented member.
sipRule_targetApplications :: Lens.Lens' SipRule (Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication))
sipRule_targetApplications = Lens.lens (\SipRule' {targetApplications} -> targetApplications) (\s@SipRule' {} a -> s {targetApplications = a} :: SipRule) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
sipRule_triggerType :: Lens.Lens' SipRule (Prelude.Maybe SipRuleTriggerType)
sipRule_triggerType = Lens.lens (\SipRule' {triggerType} -> triggerType) (\s@SipRule' {} a -> s {triggerType = a} :: SipRule)

-- | Undocumented member.
sipRule_triggerValue :: Lens.Lens' SipRule (Prelude.Maybe Prelude.Text)
sipRule_triggerValue = Lens.lens (\SipRule' {triggerValue} -> triggerValue) (\s@SipRule' {} a -> s {triggerValue = a} :: SipRule)

-- | Undocumented member.
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
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sipRuleId
      `Prelude.seq` Prelude.rnf targetApplications
      `Prelude.seq` Prelude.rnf triggerType
      `Prelude.seq` Prelude.rnf triggerValue
      `Prelude.seq` Prelude.rnf updatedTimestamp
