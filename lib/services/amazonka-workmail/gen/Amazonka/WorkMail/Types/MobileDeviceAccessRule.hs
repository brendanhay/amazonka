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
-- Module      : Amazonka.WorkMail.Types.MobileDeviceAccessRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.MobileDeviceAccessRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.MobileDeviceAccessRuleEffect

-- | A rule that controls access to mobile devices for an WorkMail group.
--
-- /See:/ 'newMobileDeviceAccessRule' smart constructor.
data MobileDeviceAccessRule = MobileDeviceAccessRule'
  { -- | The date and time at which an access rule was created.
    dateCreated :: Prelude.Maybe Data.POSIX,
    -- | The date and time at which an access rule was modified.
    dateModified :: Prelude.Maybe Data.POSIX,
    -- | The description of a mobile access rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | Device models that a rule will match.
    deviceModels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device operating systems that a rule will match.
    deviceOperatingSystems :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device types that a rule will match.
    deviceTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device user agents that a rule will match.
    deviceUserAgents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The effect of the rule when it matches. Allowed values are @ALLOW@ or
    -- @DENY@.
    effect :: Prelude.Maybe MobileDeviceAccessRuleEffect,
    -- | The ID assigned to a mobile access rule.
    mobileDeviceAccessRuleId :: Prelude.Maybe Prelude.Text,
    -- | The name of a mobile access rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | Device models that a rule __will not__ match. All other device models
    -- will match.
    notDeviceModels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device operating systems that a rule __will not__ match. All other
    -- device types will match.
    notDeviceOperatingSystems :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device types that a rule __will not__ match. All other device types will
    -- match.
    notDeviceTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device user agents that a rule __will not__ match. All other device user
    -- agents will match.
    notDeviceUserAgents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MobileDeviceAccessRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateCreated', 'mobileDeviceAccessRule_dateCreated' - The date and time at which an access rule was created.
--
-- 'dateModified', 'mobileDeviceAccessRule_dateModified' - The date and time at which an access rule was modified.
--
-- 'description', 'mobileDeviceAccessRule_description' - The description of a mobile access rule.
--
-- 'deviceModels', 'mobileDeviceAccessRule_deviceModels' - Device models that a rule will match.
--
-- 'deviceOperatingSystems', 'mobileDeviceAccessRule_deviceOperatingSystems' - Device operating systems that a rule will match.
--
-- 'deviceTypes', 'mobileDeviceAccessRule_deviceTypes' - Device types that a rule will match.
--
-- 'deviceUserAgents', 'mobileDeviceAccessRule_deviceUserAgents' - Device user agents that a rule will match.
--
-- 'effect', 'mobileDeviceAccessRule_effect' - The effect of the rule when it matches. Allowed values are @ALLOW@ or
-- @DENY@.
--
-- 'mobileDeviceAccessRuleId', 'mobileDeviceAccessRule_mobileDeviceAccessRuleId' - The ID assigned to a mobile access rule.
--
-- 'name', 'mobileDeviceAccessRule_name' - The name of a mobile access rule.
--
-- 'notDeviceModels', 'mobileDeviceAccessRule_notDeviceModels' - Device models that a rule __will not__ match. All other device models
-- will match.
--
-- 'notDeviceOperatingSystems', 'mobileDeviceAccessRule_notDeviceOperatingSystems' - Device operating systems that a rule __will not__ match. All other
-- device types will match.
--
-- 'notDeviceTypes', 'mobileDeviceAccessRule_notDeviceTypes' - Device types that a rule __will not__ match. All other device types will
-- match.
--
-- 'notDeviceUserAgents', 'mobileDeviceAccessRule_notDeviceUserAgents' - Device user agents that a rule __will not__ match. All other device user
-- agents will match.
newMobileDeviceAccessRule ::
  MobileDeviceAccessRule
newMobileDeviceAccessRule =
  MobileDeviceAccessRule'
    { dateCreated =
        Prelude.Nothing,
      dateModified = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceModels = Prelude.Nothing,
      deviceOperatingSystems = Prelude.Nothing,
      deviceTypes = Prelude.Nothing,
      deviceUserAgents = Prelude.Nothing,
      effect = Prelude.Nothing,
      mobileDeviceAccessRuleId = Prelude.Nothing,
      name = Prelude.Nothing,
      notDeviceModels = Prelude.Nothing,
      notDeviceOperatingSystems = Prelude.Nothing,
      notDeviceTypes = Prelude.Nothing,
      notDeviceUserAgents = Prelude.Nothing
    }

-- | The date and time at which an access rule was created.
mobileDeviceAccessRule_dateCreated :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessRule_dateCreated = Lens.lens (\MobileDeviceAccessRule' {dateCreated} -> dateCreated) (\s@MobileDeviceAccessRule' {} a -> s {dateCreated = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Data._Time

-- | The date and time at which an access rule was modified.
mobileDeviceAccessRule_dateModified :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessRule_dateModified = Lens.lens (\MobileDeviceAccessRule' {dateModified} -> dateModified) (\s@MobileDeviceAccessRule' {} a -> s {dateModified = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Data._Time

-- | The description of a mobile access rule.
mobileDeviceAccessRule_description :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessRule_description = Lens.lens (\MobileDeviceAccessRule' {description} -> description) (\s@MobileDeviceAccessRule' {} a -> s {description = a} :: MobileDeviceAccessRule)

-- | Device models that a rule will match.
mobileDeviceAccessRule_deviceModels :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceModels = Lens.lens (\MobileDeviceAccessRule' {deviceModels} -> deviceModels) (\s@MobileDeviceAccessRule' {} a -> s {deviceModels = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device operating systems that a rule will match.
mobileDeviceAccessRule_deviceOperatingSystems :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceOperatingSystems = Lens.lens (\MobileDeviceAccessRule' {deviceOperatingSystems} -> deviceOperatingSystems) (\s@MobileDeviceAccessRule' {} a -> s {deviceOperatingSystems = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device types that a rule will match.
mobileDeviceAccessRule_deviceTypes :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceTypes = Lens.lens (\MobileDeviceAccessRule' {deviceTypes} -> deviceTypes) (\s@MobileDeviceAccessRule' {} a -> s {deviceTypes = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device user agents that a rule will match.
mobileDeviceAccessRule_deviceUserAgents :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceUserAgents = Lens.lens (\MobileDeviceAccessRule' {deviceUserAgents} -> deviceUserAgents) (\s@MobileDeviceAccessRule' {} a -> s {deviceUserAgents = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | The effect of the rule when it matches. Allowed values are @ALLOW@ or
-- @DENY@.
mobileDeviceAccessRule_effect :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe MobileDeviceAccessRuleEffect)
mobileDeviceAccessRule_effect = Lens.lens (\MobileDeviceAccessRule' {effect} -> effect) (\s@MobileDeviceAccessRule' {} a -> s {effect = a} :: MobileDeviceAccessRule)

-- | The ID assigned to a mobile access rule.
mobileDeviceAccessRule_mobileDeviceAccessRuleId :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessRule_mobileDeviceAccessRuleId = Lens.lens (\MobileDeviceAccessRule' {mobileDeviceAccessRuleId} -> mobileDeviceAccessRuleId) (\s@MobileDeviceAccessRule' {} a -> s {mobileDeviceAccessRuleId = a} :: MobileDeviceAccessRule)

-- | The name of a mobile access rule.
mobileDeviceAccessRule_name :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessRule_name = Lens.lens (\MobileDeviceAccessRule' {name} -> name) (\s@MobileDeviceAccessRule' {} a -> s {name = a} :: MobileDeviceAccessRule)

-- | Device models that a rule __will not__ match. All other device models
-- will match.
mobileDeviceAccessRule_notDeviceModels :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceModels = Lens.lens (\MobileDeviceAccessRule' {notDeviceModels} -> notDeviceModels) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceModels = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device operating systems that a rule __will not__ match. All other
-- device types will match.
mobileDeviceAccessRule_notDeviceOperatingSystems :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceOperatingSystems = Lens.lens (\MobileDeviceAccessRule' {notDeviceOperatingSystems} -> notDeviceOperatingSystems) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceOperatingSystems = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device types that a rule __will not__ match. All other device types will
-- match.
mobileDeviceAccessRule_notDeviceTypes :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceTypes = Lens.lens (\MobileDeviceAccessRule' {notDeviceTypes} -> notDeviceTypes) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceTypes = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device user agents that a rule __will not__ match. All other device user
-- agents will match.
mobileDeviceAccessRule_notDeviceUserAgents :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceUserAgents = Lens.lens (\MobileDeviceAccessRule' {notDeviceUserAgents} -> notDeviceUserAgents) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceUserAgents = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MobileDeviceAccessRule where
  parseJSON =
    Data.withObject
      "MobileDeviceAccessRule"
      ( \x ->
          MobileDeviceAccessRule'
            Prelude.<$> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "DateModified")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DeviceModels")
            Prelude.<*> (x Data..:? "DeviceOperatingSystems")
            Prelude.<*> (x Data..:? "DeviceTypes")
            Prelude.<*> (x Data..:? "DeviceUserAgents")
            Prelude.<*> (x Data..:? "Effect")
            Prelude.<*> (x Data..:? "MobileDeviceAccessRuleId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NotDeviceModels")
            Prelude.<*> (x Data..:? "NotDeviceOperatingSystems")
            Prelude.<*> (x Data..:? "NotDeviceTypes")
            Prelude.<*> (x Data..:? "NotDeviceUserAgents")
      )

instance Prelude.Hashable MobileDeviceAccessRule where
  hashWithSalt _salt MobileDeviceAccessRule' {..} =
    _salt
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateModified
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceModels
      `Prelude.hashWithSalt` deviceOperatingSystems
      `Prelude.hashWithSalt` deviceTypes
      `Prelude.hashWithSalt` deviceUserAgents
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` mobileDeviceAccessRuleId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notDeviceModels
      `Prelude.hashWithSalt` notDeviceOperatingSystems
      `Prelude.hashWithSalt` notDeviceTypes
      `Prelude.hashWithSalt` notDeviceUserAgents

instance Prelude.NFData MobileDeviceAccessRule where
  rnf MobileDeviceAccessRule' {..} =
    Prelude.rnf dateCreated `Prelude.seq`
      Prelude.rnf dateModified `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf deviceModels `Prelude.seq`
            Prelude.rnf deviceOperatingSystems `Prelude.seq`
              Prelude.rnf deviceTypes `Prelude.seq`
                Prelude.rnf deviceUserAgents `Prelude.seq`
                  Prelude.rnf effect `Prelude.seq`
                    Prelude.rnf mobileDeviceAccessRuleId `Prelude.seq`
                      Prelude.rnf name `Prelude.seq`
                        Prelude.rnf notDeviceModels `Prelude.seq`
                          Prelude.rnf notDeviceOperatingSystems `Prelude.seq`
                            Prelude.rnf notDeviceTypes `Prelude.seq`
                              Prelude.rnf notDeviceUserAgents
