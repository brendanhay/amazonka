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
-- Module      : Amazonka.WorkMail.Types.MobileDeviceAccessMatchedRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.MobileDeviceAccessMatchedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The rule that a simulated user matches.
--
-- /See:/ 'newMobileDeviceAccessMatchedRule' smart constructor.
data MobileDeviceAccessMatchedRule = MobileDeviceAccessMatchedRule'
  { -- | Identifier of the rule that a simulated user matches.
    mobileDeviceAccessRuleId :: Prelude.Maybe Prelude.Text,
    -- | Name of a rule that a simulated user matches.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MobileDeviceAccessMatchedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mobileDeviceAccessRuleId', 'mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId' - Identifier of the rule that a simulated user matches.
--
-- 'name', 'mobileDeviceAccessMatchedRule_name' - Name of a rule that a simulated user matches.
newMobileDeviceAccessMatchedRule ::
  MobileDeviceAccessMatchedRule
newMobileDeviceAccessMatchedRule =
  MobileDeviceAccessMatchedRule'
    { mobileDeviceAccessRuleId =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Identifier of the rule that a simulated user matches.
mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId :: Lens.Lens' MobileDeviceAccessMatchedRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessMatchedRule_mobileDeviceAccessRuleId = Lens.lens (\MobileDeviceAccessMatchedRule' {mobileDeviceAccessRuleId} -> mobileDeviceAccessRuleId) (\s@MobileDeviceAccessMatchedRule' {} a -> s {mobileDeviceAccessRuleId = a} :: MobileDeviceAccessMatchedRule)

-- | Name of a rule that a simulated user matches.
mobileDeviceAccessMatchedRule_name :: Lens.Lens' MobileDeviceAccessMatchedRule (Prelude.Maybe Prelude.Text)
mobileDeviceAccessMatchedRule_name = Lens.lens (\MobileDeviceAccessMatchedRule' {name} -> name) (\s@MobileDeviceAccessMatchedRule' {} a -> s {name = a} :: MobileDeviceAccessMatchedRule)

instance Data.FromJSON MobileDeviceAccessMatchedRule where
  parseJSON =
    Data.withObject
      "MobileDeviceAccessMatchedRule"
      ( \x ->
          MobileDeviceAccessMatchedRule'
            Prelude.<$> (x Data..:? "MobileDeviceAccessRuleId")
            Prelude.<*> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    MobileDeviceAccessMatchedRule
  where
  hashWithSalt _salt MobileDeviceAccessMatchedRule' {..} =
    _salt
      `Prelude.hashWithSalt` mobileDeviceAccessRuleId
      `Prelude.hashWithSalt` name

instance Prelude.NFData MobileDeviceAccessMatchedRule where
  rnf MobileDeviceAccessMatchedRule' {..} =
    Prelude.rnf mobileDeviceAccessRuleId
      `Prelude.seq` Prelude.rnf name
