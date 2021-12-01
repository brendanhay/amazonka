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
-- Module      : Amazonka.SecurityHub.Types.StandardsControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ControlStatus
import Amazonka.SecurityHub.Types.SeverityRating

-- | Details for an individual security standard control.
--
-- /See:/ 'newStandardsControl' smart constructor.
data StandardsControl = StandardsControl'
  { -- | A link to remediation information for the control in the Security Hub
    -- user documentation.
    remediationUrl :: Prelude.Maybe Prelude.Text,
    -- | The severity of findings generated from this security standard control.
    --
    -- The finding severity is based on an assessment of how easy it would be
    -- to compromise Amazon Web Services resources if the issue is detected.
    severityRating :: Prelude.Maybe SeverityRating,
    -- | The date and time that the status of the security standard control was
    -- most recently updated.
    controlStatusUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The list of requirements that are related to this control.
    relatedRequirements :: Prelude.Maybe [Prelude.Text],
    -- | The current status of the security standard control. Indicates whether
    -- the control is enabled or disabled. Security Hub does not check against
    -- disabled controls.
    controlStatus :: Prelude.Maybe ControlStatus,
    -- | The reason provided for the most recent change in status for the
    -- control.
    disabledReason :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the security standard control.
    controlId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the security standard control.
    standardsControlArn :: Prelude.Maybe Prelude.Text,
    -- | The title of the security standard control.
    title :: Prelude.Maybe Prelude.Text,
    -- | The longer description of the security standard control. Provides
    -- information about what the control is checking for.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remediationUrl', 'standardsControl_remediationUrl' - A link to remediation information for the control in the Security Hub
-- user documentation.
--
-- 'severityRating', 'standardsControl_severityRating' - The severity of findings generated from this security standard control.
--
-- The finding severity is based on an assessment of how easy it would be
-- to compromise Amazon Web Services resources if the issue is detected.
--
-- 'controlStatusUpdatedAt', 'standardsControl_controlStatusUpdatedAt' - The date and time that the status of the security standard control was
-- most recently updated.
--
-- 'relatedRequirements', 'standardsControl_relatedRequirements' - The list of requirements that are related to this control.
--
-- 'controlStatus', 'standardsControl_controlStatus' - The current status of the security standard control. Indicates whether
-- the control is enabled or disabled. Security Hub does not check against
-- disabled controls.
--
-- 'disabledReason', 'standardsControl_disabledReason' - The reason provided for the most recent change in status for the
-- control.
--
-- 'controlId', 'standardsControl_controlId' - The identifier of the security standard control.
--
-- 'standardsControlArn', 'standardsControl_standardsControlArn' - The ARN of the security standard control.
--
-- 'title', 'standardsControl_title' - The title of the security standard control.
--
-- 'description', 'standardsControl_description' - The longer description of the security standard control. Provides
-- information about what the control is checking for.
newStandardsControl ::
  StandardsControl
newStandardsControl =
  StandardsControl'
    { remediationUrl = Prelude.Nothing,
      severityRating = Prelude.Nothing,
      controlStatusUpdatedAt = Prelude.Nothing,
      relatedRequirements = Prelude.Nothing,
      controlStatus = Prelude.Nothing,
      disabledReason = Prelude.Nothing,
      controlId = Prelude.Nothing,
      standardsControlArn = Prelude.Nothing,
      title = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A link to remediation information for the control in the Security Hub
-- user documentation.
standardsControl_remediationUrl :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.Text)
standardsControl_remediationUrl = Lens.lens (\StandardsControl' {remediationUrl} -> remediationUrl) (\s@StandardsControl' {} a -> s {remediationUrl = a} :: StandardsControl)

-- | The severity of findings generated from this security standard control.
--
-- The finding severity is based on an assessment of how easy it would be
-- to compromise Amazon Web Services resources if the issue is detected.
standardsControl_severityRating :: Lens.Lens' StandardsControl (Prelude.Maybe SeverityRating)
standardsControl_severityRating = Lens.lens (\StandardsControl' {severityRating} -> severityRating) (\s@StandardsControl' {} a -> s {severityRating = a} :: StandardsControl)

-- | The date and time that the status of the security standard control was
-- most recently updated.
standardsControl_controlStatusUpdatedAt :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.UTCTime)
standardsControl_controlStatusUpdatedAt = Lens.lens (\StandardsControl' {controlStatusUpdatedAt} -> controlStatusUpdatedAt) (\s@StandardsControl' {} a -> s {controlStatusUpdatedAt = a} :: StandardsControl) Prelude.. Lens.mapping Core._Time

-- | The list of requirements that are related to this control.
standardsControl_relatedRequirements :: Lens.Lens' StandardsControl (Prelude.Maybe [Prelude.Text])
standardsControl_relatedRequirements = Lens.lens (\StandardsControl' {relatedRequirements} -> relatedRequirements) (\s@StandardsControl' {} a -> s {relatedRequirements = a} :: StandardsControl) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the security standard control. Indicates whether
-- the control is enabled or disabled. Security Hub does not check against
-- disabled controls.
standardsControl_controlStatus :: Lens.Lens' StandardsControl (Prelude.Maybe ControlStatus)
standardsControl_controlStatus = Lens.lens (\StandardsControl' {controlStatus} -> controlStatus) (\s@StandardsControl' {} a -> s {controlStatus = a} :: StandardsControl)

-- | The reason provided for the most recent change in status for the
-- control.
standardsControl_disabledReason :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.Text)
standardsControl_disabledReason = Lens.lens (\StandardsControl' {disabledReason} -> disabledReason) (\s@StandardsControl' {} a -> s {disabledReason = a} :: StandardsControl)

-- | The identifier of the security standard control.
standardsControl_controlId :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.Text)
standardsControl_controlId = Lens.lens (\StandardsControl' {controlId} -> controlId) (\s@StandardsControl' {} a -> s {controlId = a} :: StandardsControl)

-- | The ARN of the security standard control.
standardsControl_standardsControlArn :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.Text)
standardsControl_standardsControlArn = Lens.lens (\StandardsControl' {standardsControlArn} -> standardsControlArn) (\s@StandardsControl' {} a -> s {standardsControlArn = a} :: StandardsControl)

-- | The title of the security standard control.
standardsControl_title :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.Text)
standardsControl_title = Lens.lens (\StandardsControl' {title} -> title) (\s@StandardsControl' {} a -> s {title = a} :: StandardsControl)

-- | The longer description of the security standard control. Provides
-- information about what the control is checking for.
standardsControl_description :: Lens.Lens' StandardsControl (Prelude.Maybe Prelude.Text)
standardsControl_description = Lens.lens (\StandardsControl' {description} -> description) (\s@StandardsControl' {} a -> s {description = a} :: StandardsControl)

instance Core.FromJSON StandardsControl where
  parseJSON =
    Core.withObject
      "StandardsControl"
      ( \x ->
          StandardsControl'
            Prelude.<$> (x Core..:? "RemediationUrl")
            Prelude.<*> (x Core..:? "SeverityRating")
            Prelude.<*> (x Core..:? "ControlStatusUpdatedAt")
            Prelude.<*> ( x Core..:? "RelatedRequirements"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ControlStatus")
            Prelude.<*> (x Core..:? "DisabledReason")
            Prelude.<*> (x Core..:? "ControlId")
            Prelude.<*> (x Core..:? "StandardsControlArn")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable StandardsControl where
  hashWithSalt salt' StandardsControl' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` standardsControlArn
      `Prelude.hashWithSalt` controlId
      `Prelude.hashWithSalt` disabledReason
      `Prelude.hashWithSalt` controlStatus
      `Prelude.hashWithSalt` relatedRequirements
      `Prelude.hashWithSalt` controlStatusUpdatedAt
      `Prelude.hashWithSalt` severityRating
      `Prelude.hashWithSalt` remediationUrl

instance Prelude.NFData StandardsControl where
  rnf StandardsControl' {..} =
    Prelude.rnf remediationUrl
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf standardsControlArn
      `Prelude.seq` Prelude.rnf controlId
      `Prelude.seq` Prelude.rnf disabledReason
      `Prelude.seq` Prelude.rnf controlStatus
      `Prelude.seq` Prelude.rnf relatedRequirements
      `Prelude.seq` Prelude.rnf controlStatusUpdatedAt
      `Prelude.seq` Prelude.rnf severityRating
