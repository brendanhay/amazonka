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
-- Module      : Amazonka.SecurityHub.Types.Compliance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Compliance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ComplianceStatus
import Amazonka.SecurityHub.Types.StatusReason

-- | Contains finding details that are specific to control-based findings.
-- Only returned for findings generated from controls.
--
-- /See:/ 'newCompliance' smart constructor.
data Compliance = Compliance'
  { -- | The result of a standards check.
    --
    -- The valid values for @Status@ are as follows.
    --
    -- -   -   @PASSED@ - Standards check passed for all evaluated resources.
    --
    --     -   @WARNING@ - Some information is missing or this check is not
    --         supported for your configuration.
    --
    --     -   @FAILED@ - Standards check failed for at least one evaluated
    --         resource.
    --
    --     -   @NOT_AVAILABLE@ - Check could not be performed due to a service
    --         outage, API error, or because the result of the Config
    --         evaluation was @NOT_APPLICABLE@. If the Config evaluation result
    --         was @NOT_APPLICABLE@, then after 3 days, Security Hub
    --         automatically archives the finding.
    status :: Prelude.Maybe ComplianceStatus,
    -- | For a control, the industry or regulatory framework requirements that
    -- are related to the control. The check for that control is aligned with
    -- these requirements.
    relatedRequirements :: Prelude.Maybe [Prelude.Text],
    -- | For findings generated from controls, a list of reasons behind the value
    -- of @Status@. For the list of status reason codes and their meanings, see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
    -- in the /Security Hub User Guide/.
    statusReasons :: Prelude.Maybe [StatusReason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Compliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'compliance_status' - The result of a standards check.
--
-- The valid values for @Status@ are as follows.
--
-- -   -   @PASSED@ - Standards check passed for all evaluated resources.
--
--     -   @WARNING@ - Some information is missing or this check is not
--         supported for your configuration.
--
--     -   @FAILED@ - Standards check failed for at least one evaluated
--         resource.
--
--     -   @NOT_AVAILABLE@ - Check could not be performed due to a service
--         outage, API error, or because the result of the Config
--         evaluation was @NOT_APPLICABLE@. If the Config evaluation result
--         was @NOT_APPLICABLE@, then after 3 days, Security Hub
--         automatically archives the finding.
--
-- 'relatedRequirements', 'compliance_relatedRequirements' - For a control, the industry or regulatory framework requirements that
-- are related to the control. The check for that control is aligned with
-- these requirements.
--
-- 'statusReasons', 'compliance_statusReasons' - For findings generated from controls, a list of reasons behind the value
-- of @Status@. For the list of status reason codes and their meanings, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
-- in the /Security Hub User Guide/.
newCompliance ::
  Compliance
newCompliance =
  Compliance'
    { status = Prelude.Nothing,
      relatedRequirements = Prelude.Nothing,
      statusReasons = Prelude.Nothing
    }

-- | The result of a standards check.
--
-- The valid values for @Status@ are as follows.
--
-- -   -   @PASSED@ - Standards check passed for all evaluated resources.
--
--     -   @WARNING@ - Some information is missing or this check is not
--         supported for your configuration.
--
--     -   @FAILED@ - Standards check failed for at least one evaluated
--         resource.
--
--     -   @NOT_AVAILABLE@ - Check could not be performed due to a service
--         outage, API error, or because the result of the Config
--         evaluation was @NOT_APPLICABLE@. If the Config evaluation result
--         was @NOT_APPLICABLE@, then after 3 days, Security Hub
--         automatically archives the finding.
compliance_status :: Lens.Lens' Compliance (Prelude.Maybe ComplianceStatus)
compliance_status = Lens.lens (\Compliance' {status} -> status) (\s@Compliance' {} a -> s {status = a} :: Compliance)

-- | For a control, the industry or regulatory framework requirements that
-- are related to the control. The check for that control is aligned with
-- these requirements.
compliance_relatedRequirements :: Lens.Lens' Compliance (Prelude.Maybe [Prelude.Text])
compliance_relatedRequirements = Lens.lens (\Compliance' {relatedRequirements} -> relatedRequirements) (\s@Compliance' {} a -> s {relatedRequirements = a} :: Compliance) Prelude.. Lens.mapping Lens.coerced

-- | For findings generated from controls, a list of reasons behind the value
-- of @Status@. For the list of status reason codes and their meanings, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
-- in the /Security Hub User Guide/.
compliance_statusReasons :: Lens.Lens' Compliance (Prelude.Maybe [StatusReason])
compliance_statusReasons = Lens.lens (\Compliance' {statusReasons} -> statusReasons) (\s@Compliance' {} a -> s {statusReasons = a} :: Compliance) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Compliance where
  parseJSON =
    Core.withObject
      "Compliance"
      ( \x ->
          Compliance'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> ( x Core..:? "RelatedRequirements"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StatusReasons" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Compliance where
  hashWithSalt salt' Compliance' {..} =
    salt' `Prelude.hashWithSalt` statusReasons
      `Prelude.hashWithSalt` relatedRequirements
      `Prelude.hashWithSalt` status

instance Prelude.NFData Compliance where
  rnf Compliance' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReasons
      `Prelude.seq` Prelude.rnf relatedRequirements

instance Core.ToJSON Compliance where
  toJSON Compliance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("RelatedRequirements" Core..=)
              Prelude.<$> relatedRequirements,
            ("StatusReasons" Core..=) Prelude.<$> statusReasons
          ]
      )
