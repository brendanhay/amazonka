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
-- Module      : Network.AWS.WorkMail.Types.MobileDeviceAccessRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MobileDeviceAccessRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.MobileDeviceAccessRuleEffect

-- | A rule that controls access to mobile devices for an Amazon WorkMail
-- group.
--
-- /See:/ 'newMobileDeviceAccessRule' smart constructor.
data MobileDeviceAccessRule = MobileDeviceAccessRule'
  { -- | The effect of the rule when it matches. Allowed values are @ALLOW@ or
    -- @DENY@.
    effect :: Prelude.Maybe MobileDeviceAccessRuleEffect,
    -- | Device user agents that a rule will match.
    deviceUserAgents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device types that a rule will match.
    deviceTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device types that a rule __will not__ match. All other device types will
    -- match.
    notDeviceTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device operating systems that a rule __will not__ match. All other
    -- device types will match.
    notDeviceOperatingSystems :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The date and time at which an access rule was created.
    dateCreated :: Prelude.Maybe Core.POSIX,
    -- | Device models that a rule will match.
    deviceModels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID assigned to a mobile access rule.
    mobileDeviceAccessRuleId :: Prelude.Maybe Prelude.Text,
    -- | The name of a mobile access rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which an access rule was modified.
    dateModified :: Prelude.Maybe Core.POSIX,
    -- | Device operating systems that a rule will match.
    deviceOperatingSystems :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The description of a mobile access rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | Device user agents that a rule __will not__ match. All other device user
    -- agents will match.
    notDeviceUserAgents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Device models that a rule __will not__ match. All other device models
    -- will match.
    notDeviceModels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'effect', 'mobileDeviceAccessRule_effect' - The effect of the rule when it matches. Allowed values are @ALLOW@ or
-- @DENY@.
--
-- 'deviceUserAgents', 'mobileDeviceAccessRule_deviceUserAgents' - Device user agents that a rule will match.
--
-- 'deviceTypes', 'mobileDeviceAccessRule_deviceTypes' - Device types that a rule will match.
--
-- 'notDeviceTypes', 'mobileDeviceAccessRule_notDeviceTypes' - Device types that a rule __will not__ match. All other device types will
-- match.
--
-- 'notDeviceOperatingSystems', 'mobileDeviceAccessRule_notDeviceOperatingSystems' - Device operating systems that a rule __will not__ match. All other
-- device types will match.
--
-- 'dateCreated', 'mobileDeviceAccessRule_dateCreated' - The date and time at which an access rule was created.
--
-- 'deviceModels', 'mobileDeviceAccessRule_deviceModels' - Device models that a rule will match.
--
-- 'mobileDeviceAccessRuleId', 'mobileDeviceAccessRule_mobileDeviceAccessRuleId' - The ID assigned to a mobile access rule.
--
-- 'name', 'mobileDeviceAccessRule_name' - The name of a mobile access rule.
--
-- 'dateModified', 'mobileDeviceAccessRule_dateModified' - The date and time at which an access rule was modified.
--
-- 'deviceOperatingSystems', 'mobileDeviceAccessRule_deviceOperatingSystems' - Device operating systems that a rule will match.
--
-- 'description', 'mobileDeviceAccessRule_description' - The description of a mobile access rule.
--
-- 'notDeviceUserAgents', 'mobileDeviceAccessRule_notDeviceUserAgents' - Device user agents that a rule __will not__ match. All other device user
-- agents will match.
--
-- 'notDeviceModels', 'mobileDeviceAccessRule_notDeviceModels' - Device models that a rule __will not__ match. All other device models
-- will match.
newMobileDeviceAccessRule ::
  MobileDeviceAccessRule
newMobileDeviceAccessRule =
  MobileDeviceAccessRule'
    { effect = Prelude.Nothing,
      deviceUserAgents = Prelude.Nothing,
      deviceTypes = Prelude.Nothing,
      notDeviceTypes = Prelude.Nothing,
      notDeviceOperatingSystems = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      deviceModels = Prelude.Nothing,
      mobileDeviceAccessRuleId = Prelude.Nothing,
      name = Prelude.Nothing,
      dateModified = Prelude.Nothing,
      deviceOperatingSystems = Prelude.Nothing,
      description = Prelude.Nothing,
      notDeviceUserAgents = Prelude.Nothing,
      notDeviceModels = Prelude.Nothing
    }

-- | The effect of the rule when it matches. Allowed values are @ALLOW@ or
-- @DENY@.
mobileDeviceAccessRule_effect :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe MobileDeviceAccessRuleEffect)
mobileDeviceAccessRule_effect = Lens.lens (\MobileDeviceAccessRule' {effect} -> effect) (\s@MobileDeviceAccessRule' {} a -> s {effect = a} :: MobileDeviceAccessRule)

-- | Device user agents that a rule will match.
mobileDeviceAccessRule_deviceUserAgents :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceUserAgents = Lens.lens (\MobileDeviceAccessRule' {deviceUserAgents} -> deviceUserAgents) (\s@MobileDeviceAccessRule' {} a -> s {deviceUserAgents = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device types that a rule will match.
mobileDeviceAccessRule_deviceTypes :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceTypes = Lens.lens (\MobileDeviceAccessRule' {deviceTypes} -> deviceTypes) (\s@MobileDeviceAccessRule' {} a -> s {deviceTypes = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device types that a rule __will not__ match. All other device types will
-- match.
mobileDeviceAccessRule_notDeviceTypes :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceTypes = Lens.lens (\MobileDeviceAccessRule' {notDeviceTypes} -> notDeviceTypes) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceTypes = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device operating systems that a rule __will not__ match. All other
-- device types will match.
mobileDeviceAccessRule_notDeviceOperatingSystems :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceOperatingSystems = Lens.lens (\MobileDeviceAccessRule' {notDeviceOperatingSystems} -> notDeviceOperatingSystems) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceOperatingSystems = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | The date and time at which an access rule was created.
mobileDeviceAccessRule_dateCreated :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessRule_dateCreated = Lens.lens (\MobileDeviceAccessRule' {dateCreated} -> dateCreated) (\s@MobileDeviceAccessRule' {} a -> s {dateCreated = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Core._Time

-- | Device models that a rule will match.
mobileDeviceAccessRule_deviceModels :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceModels = Lens.lens (\MobileDeviceAccessRule' {deviceModels} -> deviceModels) (\s@MobileDeviceAccessRule' {} a -> s {deviceModels = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | The ID assigned to a mobile access rule.
mobileDeviceAccessRule_mobileDeviceAccessRuleId :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessRule_mobileDeviceAccessRuleId = Lens.lens (\MobileDeviceAccessRule' {mobileDeviceAccessRuleId} -> mobileDeviceAccessRuleId) (\s@MobileDeviceAccessRule' {} a -> s {mobileDeviceAccessRuleId = a} :: MobileDeviceAccessRule)

-- | The name of a mobile access rule.
mobileDeviceAccessRule_name :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessRule_name = Lens.lens (\MobileDeviceAccessRule' {name} -> name) (\s@MobileDeviceAccessRule' {} a -> s {name = a} :: MobileDeviceAccessRule)

-- | The date and time at which an access rule was modified.
mobileDeviceAccessRule_dateModified :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessRule_dateModified = Lens.lens (\MobileDeviceAccessRule' {dateModified} -> dateModified) (\s@MobileDeviceAccessRule' {} a -> s {dateModified = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Core._Time

-- | Device operating systems that a rule will match.
mobileDeviceAccessRule_deviceOperatingSystems :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_deviceOperatingSystems = Lens.lens (\MobileDeviceAccessRule' {deviceOperatingSystems} -> deviceOperatingSystems) (\s@MobileDeviceAccessRule' {} a -> s {deviceOperatingSystems = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | The description of a mobile access rule.
mobileDeviceAccessRule_description :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessRule_description = Lens.lens (\MobileDeviceAccessRule' {description} -> description) (\s@MobileDeviceAccessRule' {} a -> s {description = a} :: MobileDeviceAccessRule)

-- | Device user agents that a rule __will not__ match. All other device user
-- agents will match.
mobileDeviceAccessRule_notDeviceUserAgents :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceUserAgents = Lens.lens (\MobileDeviceAccessRule' {notDeviceUserAgents} -> notDeviceUserAgents) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceUserAgents = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

-- | Device models that a rule __will not__ match. All other device models
-- will match.
mobileDeviceAccessRule_notDeviceModels :: Lens.Lens' MobileDeviceAccessRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
mobileDeviceAccessRule_notDeviceModels = Lens.lens (\MobileDeviceAccessRule' {notDeviceModels} -> notDeviceModels) (\s@MobileDeviceAccessRule' {} a -> s {notDeviceModels = a} :: MobileDeviceAccessRule) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MobileDeviceAccessRule where
  parseJSON =
    Core.withObject
      "MobileDeviceAccessRule"
      ( \x ->
          MobileDeviceAccessRule'
            Prelude.<$> (x Core..:? "Effect")
            Prelude.<*> (x Core..:? "DeviceUserAgents")
            Prelude.<*> (x Core..:? "DeviceTypes")
            Prelude.<*> (x Core..:? "NotDeviceTypes")
            Prelude.<*> (x Core..:? "NotDeviceOperatingSystems")
            Prelude.<*> (x Core..:? "DateCreated")
            Prelude.<*> (x Core..:? "DeviceModels")
            Prelude.<*> (x Core..:? "MobileDeviceAccessRuleId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DateModified")
            Prelude.<*> (x Core..:? "DeviceOperatingSystems")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "NotDeviceUserAgents")
            Prelude.<*> (x Core..:? "NotDeviceModels")
      )

instance Prelude.Hashable MobileDeviceAccessRule

instance Prelude.NFData MobileDeviceAccessRule
