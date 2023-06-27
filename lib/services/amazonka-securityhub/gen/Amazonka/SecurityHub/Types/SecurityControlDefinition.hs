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
-- Module      : Amazonka.SecurityHub.Types.SecurityControlDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SecurityControlDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RegionAvailabilityStatus
import Amazonka.SecurityHub.Types.SeverityRating

-- | Provides metadata for a security control, including its unique
-- standard-agnostic identifier, title, description, severity, availability
-- in Amazon Web Services Regions, and a link to remediation steps.
--
-- /See:/ 'newSecurityControlDefinition' smart constructor.
data SecurityControlDefinition = SecurityControlDefinition'
  { -- | The unique identifier of a security control across standards. Values for
    -- this field typically consist of an Amazon Web Service name and a number
    -- (for example, APIGateway.3). This parameter differs from
    -- @SecurityControlArn@, which is a unique Amazon Resource Name (ARN)
    -- assigned to a control. The ARN references the security control ID (for
    -- example,
    -- arn:aws:securityhub:eu-central-1:123456789012:security-control\/APIGateway.3).
    securityControlId :: Prelude.Text,
    -- | The title of a security control.
    title :: Prelude.Text,
    -- | The description of a security control across standards. This typically
    -- summarizes how Security Hub evaluates the control and the conditions
    -- under which it produces a failed finding. This parameter doesn\'t
    -- reference a specific standard.
    description :: Prelude.Text,
    -- | A link to Security Hub documentation that explains how to remediate a
    -- failed finding for a security control.
    remediationUrl :: Prelude.Text,
    -- | The severity of a security control. For more information about how
    -- Security Hub determines control severity, see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-findings-create-update.html#control-findings-severity Assigning severity to control findings>
    -- in the /Security Hub User Guide/.
    severityRating :: SeverityRating,
    -- | Specifies whether a security control is available in the current Amazon
    -- Web Services Region.
    currentRegionAvailability :: RegionAvailabilityStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityControlDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityControlId', 'securityControlDefinition_securityControlId' - The unique identifier of a security control across standards. Values for
-- this field typically consist of an Amazon Web Service name and a number
-- (for example, APIGateway.3). This parameter differs from
-- @SecurityControlArn@, which is a unique Amazon Resource Name (ARN)
-- assigned to a control. The ARN references the security control ID (for
-- example,
-- arn:aws:securityhub:eu-central-1:123456789012:security-control\/APIGateway.3).
--
-- 'title', 'securityControlDefinition_title' - The title of a security control.
--
-- 'description', 'securityControlDefinition_description' - The description of a security control across standards. This typically
-- summarizes how Security Hub evaluates the control and the conditions
-- under which it produces a failed finding. This parameter doesn\'t
-- reference a specific standard.
--
-- 'remediationUrl', 'securityControlDefinition_remediationUrl' - A link to Security Hub documentation that explains how to remediate a
-- failed finding for a security control.
--
-- 'severityRating', 'securityControlDefinition_severityRating' - The severity of a security control. For more information about how
-- Security Hub determines control severity, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-findings-create-update.html#control-findings-severity Assigning severity to control findings>
-- in the /Security Hub User Guide/.
--
-- 'currentRegionAvailability', 'securityControlDefinition_currentRegionAvailability' - Specifies whether a security control is available in the current Amazon
-- Web Services Region.
newSecurityControlDefinition ::
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'remediationUrl'
  Prelude.Text ->
  -- | 'severityRating'
  SeverityRating ->
  -- | 'currentRegionAvailability'
  RegionAvailabilityStatus ->
  SecurityControlDefinition
newSecurityControlDefinition
  pSecurityControlId_
  pTitle_
  pDescription_
  pRemediationUrl_
  pSeverityRating_
  pCurrentRegionAvailability_ =
    SecurityControlDefinition'
      { securityControlId =
          pSecurityControlId_,
        title = pTitle_,
        description = pDescription_,
        remediationUrl = pRemediationUrl_,
        severityRating = pSeverityRating_,
        currentRegionAvailability =
          pCurrentRegionAvailability_
      }

-- | The unique identifier of a security control across standards. Values for
-- this field typically consist of an Amazon Web Service name and a number
-- (for example, APIGateway.3). This parameter differs from
-- @SecurityControlArn@, which is a unique Amazon Resource Name (ARN)
-- assigned to a control. The ARN references the security control ID (for
-- example,
-- arn:aws:securityhub:eu-central-1:123456789012:security-control\/APIGateway.3).
securityControlDefinition_securityControlId :: Lens.Lens' SecurityControlDefinition Prelude.Text
securityControlDefinition_securityControlId = Lens.lens (\SecurityControlDefinition' {securityControlId} -> securityControlId) (\s@SecurityControlDefinition' {} a -> s {securityControlId = a} :: SecurityControlDefinition)

-- | The title of a security control.
securityControlDefinition_title :: Lens.Lens' SecurityControlDefinition Prelude.Text
securityControlDefinition_title = Lens.lens (\SecurityControlDefinition' {title} -> title) (\s@SecurityControlDefinition' {} a -> s {title = a} :: SecurityControlDefinition)

-- | The description of a security control across standards. This typically
-- summarizes how Security Hub evaluates the control and the conditions
-- under which it produces a failed finding. This parameter doesn\'t
-- reference a specific standard.
securityControlDefinition_description :: Lens.Lens' SecurityControlDefinition Prelude.Text
securityControlDefinition_description = Lens.lens (\SecurityControlDefinition' {description} -> description) (\s@SecurityControlDefinition' {} a -> s {description = a} :: SecurityControlDefinition)

-- | A link to Security Hub documentation that explains how to remediate a
-- failed finding for a security control.
securityControlDefinition_remediationUrl :: Lens.Lens' SecurityControlDefinition Prelude.Text
securityControlDefinition_remediationUrl = Lens.lens (\SecurityControlDefinition' {remediationUrl} -> remediationUrl) (\s@SecurityControlDefinition' {} a -> s {remediationUrl = a} :: SecurityControlDefinition)

-- | The severity of a security control. For more information about how
-- Security Hub determines control severity, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-findings-create-update.html#control-findings-severity Assigning severity to control findings>
-- in the /Security Hub User Guide/.
securityControlDefinition_severityRating :: Lens.Lens' SecurityControlDefinition SeverityRating
securityControlDefinition_severityRating = Lens.lens (\SecurityControlDefinition' {severityRating} -> severityRating) (\s@SecurityControlDefinition' {} a -> s {severityRating = a} :: SecurityControlDefinition)

-- | Specifies whether a security control is available in the current Amazon
-- Web Services Region.
securityControlDefinition_currentRegionAvailability :: Lens.Lens' SecurityControlDefinition RegionAvailabilityStatus
securityControlDefinition_currentRegionAvailability = Lens.lens (\SecurityControlDefinition' {currentRegionAvailability} -> currentRegionAvailability) (\s@SecurityControlDefinition' {} a -> s {currentRegionAvailability = a} :: SecurityControlDefinition)

instance Data.FromJSON SecurityControlDefinition where
  parseJSON =
    Data.withObject
      "SecurityControlDefinition"
      ( \x ->
          SecurityControlDefinition'
            Prelude.<$> (x Data..: "SecurityControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "RemediationUrl")
            Prelude.<*> (x Data..: "SeverityRating")
            Prelude.<*> (x Data..: "CurrentRegionAvailability")
      )

instance Prelude.Hashable SecurityControlDefinition where
  hashWithSalt _salt SecurityControlDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` securityControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` remediationUrl
      `Prelude.hashWithSalt` severityRating
      `Prelude.hashWithSalt` currentRegionAvailability

instance Prelude.NFData SecurityControlDefinition where
  rnf SecurityControlDefinition' {..} =
    Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf remediationUrl
      `Prelude.seq` Prelude.rnf severityRating
      `Prelude.seq` Prelude.rnf currentRegionAvailability
