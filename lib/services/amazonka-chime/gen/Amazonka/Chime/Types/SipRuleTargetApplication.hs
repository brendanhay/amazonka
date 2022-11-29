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
-- Module      : Amazonka.Chime.Types.SipRuleTargetApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SipRuleTargetApplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Target SIP media application and other details, such as priority and AWS
-- Region, to be specified in the SIP rule. Only one SIP rule per AWS
-- Region can be provided.
--
-- /See:/ 'newSipRuleTargetApplication' smart constructor.
data SipRuleTargetApplication = SipRuleTargetApplication'
  { -- | Priority of the SIP media application in the target list.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The AWS Region of the target application.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The SIP media application ID.
    sipMediaApplicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipRuleTargetApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'sipRuleTargetApplication_priority' - Priority of the SIP media application in the target list.
--
-- 'awsRegion', 'sipRuleTargetApplication_awsRegion' - The AWS Region of the target application.
--
-- 'sipMediaApplicationId', 'sipRuleTargetApplication_sipMediaApplicationId' - The SIP media application ID.
newSipRuleTargetApplication ::
  SipRuleTargetApplication
newSipRuleTargetApplication =
  SipRuleTargetApplication'
    { priority =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      sipMediaApplicationId = Prelude.Nothing
    }

-- | Priority of the SIP media application in the target list.
sipRuleTargetApplication_priority :: Lens.Lens' SipRuleTargetApplication (Prelude.Maybe Prelude.Natural)
sipRuleTargetApplication_priority = Lens.lens (\SipRuleTargetApplication' {priority} -> priority) (\s@SipRuleTargetApplication' {} a -> s {priority = a} :: SipRuleTargetApplication)

-- | The AWS Region of the target application.
sipRuleTargetApplication_awsRegion :: Lens.Lens' SipRuleTargetApplication (Prelude.Maybe Prelude.Text)
sipRuleTargetApplication_awsRegion = Lens.lens (\SipRuleTargetApplication' {awsRegion} -> awsRegion) (\s@SipRuleTargetApplication' {} a -> s {awsRegion = a} :: SipRuleTargetApplication)

-- | The SIP media application ID.
sipRuleTargetApplication_sipMediaApplicationId :: Lens.Lens' SipRuleTargetApplication (Prelude.Maybe Prelude.Text)
sipRuleTargetApplication_sipMediaApplicationId = Lens.lens (\SipRuleTargetApplication' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@SipRuleTargetApplication' {} a -> s {sipMediaApplicationId = a} :: SipRuleTargetApplication)

instance Core.FromJSON SipRuleTargetApplication where
  parseJSON =
    Core.withObject
      "SipRuleTargetApplication"
      ( \x ->
          SipRuleTargetApplication'
            Prelude.<$> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "AwsRegion")
            Prelude.<*> (x Core..:? "SipMediaApplicationId")
      )

instance Prelude.Hashable SipRuleTargetApplication where
  hashWithSalt _salt SipRuleTargetApplication' {..} =
    _salt `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` sipMediaApplicationId

instance Prelude.NFData SipRuleTargetApplication where
  rnf SipRuleTargetApplication' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf sipMediaApplicationId

instance Core.ToJSON SipRuleTargetApplication where
  toJSON SipRuleTargetApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Priority" Core..=) Prelude.<$> priority,
            ("AwsRegion" Core..=) Prelude.<$> awsRegion,
            ("SipMediaApplicationId" Core..=)
              Prelude.<$> sipMediaApplicationId
          ]
      )
