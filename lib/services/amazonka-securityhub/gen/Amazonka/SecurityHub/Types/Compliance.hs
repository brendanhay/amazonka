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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Compliance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AssociatedStandard
import Amazonka.SecurityHub.Types.ComplianceStatus
import Amazonka.SecurityHub.Types.StatusReason

-- | Contains finding details that are specific to control-based findings.
-- Only returned for findings generated from controls.
--
-- /See:/ 'newCompliance' smart constructor.
data Compliance = Compliance'
  { -- | The enabled security standards in which a security control is currently
    -- enabled.
    associatedStandards :: Prelude.Maybe [AssociatedStandard],
    -- | For a control, the industry or regulatory framework requirements that
    -- are related to the control. The check for that control is aligned with
    -- these requirements.
    relatedRequirements :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier of a control across standards. Values for this
    -- field typically consist of an Amazon Web Service and a number, such as
    -- APIGateway.5.
    securityControlId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe ComplianceStatus,
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
-- 'associatedStandards', 'compliance_associatedStandards' - The enabled security standards in which a security control is currently
-- enabled.
--
-- 'relatedRequirements', 'compliance_relatedRequirements' - For a control, the industry or regulatory framework requirements that
-- are related to the control. The check for that control is aligned with
-- these requirements.
--
-- 'securityControlId', 'compliance_securityControlId' - The unique identifier of a control across standards. Values for this
-- field typically consist of an Amazon Web Service and a number, such as
-- APIGateway.5.
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
-- 'statusReasons', 'compliance_statusReasons' - For findings generated from controls, a list of reasons behind the value
-- of @Status@. For the list of status reason codes and their meanings, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
-- in the /Security Hub User Guide/.
newCompliance ::
  Compliance
newCompliance =
  Compliance'
    { associatedStandards = Prelude.Nothing,
      relatedRequirements = Prelude.Nothing,
      securityControlId = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReasons = Prelude.Nothing
    }

-- | The enabled security standards in which a security control is currently
-- enabled.
compliance_associatedStandards :: Lens.Lens' Compliance (Prelude.Maybe [AssociatedStandard])
compliance_associatedStandards = Lens.lens (\Compliance' {associatedStandards} -> associatedStandards) (\s@Compliance' {} a -> s {associatedStandards = a} :: Compliance) Prelude.. Lens.mapping Lens.coerced

-- | For a control, the industry or regulatory framework requirements that
-- are related to the control. The check for that control is aligned with
-- these requirements.
compliance_relatedRequirements :: Lens.Lens' Compliance (Prelude.Maybe [Prelude.Text])
compliance_relatedRequirements = Lens.lens (\Compliance' {relatedRequirements} -> relatedRequirements) (\s@Compliance' {} a -> s {relatedRequirements = a} :: Compliance) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of a control across standards. Values for this
-- field typically consist of an Amazon Web Service and a number, such as
-- APIGateway.5.
compliance_securityControlId :: Lens.Lens' Compliance (Prelude.Maybe Prelude.Text)
compliance_securityControlId = Lens.lens (\Compliance' {securityControlId} -> securityControlId) (\s@Compliance' {} a -> s {securityControlId = a} :: Compliance)

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

-- | For findings generated from controls, a list of reasons behind the value
-- of @Status@. For the list of status reason codes and their meanings, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
-- in the /Security Hub User Guide/.
compliance_statusReasons :: Lens.Lens' Compliance (Prelude.Maybe [StatusReason])
compliance_statusReasons = Lens.lens (\Compliance' {statusReasons} -> statusReasons) (\s@Compliance' {} a -> s {statusReasons = a} :: Compliance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Compliance where
  parseJSON =
    Data.withObject
      "Compliance"
      ( \x ->
          Compliance'
            Prelude.<$> ( x
                            Data..:? "AssociatedStandards"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "RelatedRequirements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SecurityControlId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReasons" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Compliance where
  hashWithSalt _salt Compliance' {..} =
    _salt
      `Prelude.hashWithSalt` associatedStandards
      `Prelude.hashWithSalt` relatedRequirements
      `Prelude.hashWithSalt` securityControlId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReasons

instance Prelude.NFData Compliance where
  rnf Compliance' {..} =
    Prelude.rnf associatedStandards
      `Prelude.seq` Prelude.rnf relatedRequirements
      `Prelude.seq` Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReasons

instance Data.ToJSON Compliance where
  toJSON Compliance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociatedStandards" Data..=)
              Prelude.<$> associatedStandards,
            ("RelatedRequirements" Data..=)
              Prelude.<$> relatedRequirements,
            ("SecurityControlId" Data..=)
              Prelude.<$> securityControlId,
            ("Status" Data..=) Prelude.<$> status,
            ("StatusReasons" Data..=) Prelude.<$> statusReasons
          ]
      )
