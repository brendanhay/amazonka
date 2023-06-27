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
-- Module      : Amazonka.SecurityHub.Types.SecurityControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SecurityControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ControlStatus
import Amazonka.SecurityHub.Types.SeverityRating

-- | A security control in Security Hub describes a security best practice
-- related to a specific resource.
--
-- /See:/ 'newSecurityControl' smart constructor.
data SecurityControl = SecurityControl'
  { -- | The unique identifier of a security control across standards. Values for
    -- this field typically consist of an Amazon Web Service name and a number,
    -- such as APIGateway.3.
    securityControlId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for a security control across standards,
    -- such as
    -- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
    -- This parameter doesn\'t mention a specific standard.
    securityControlArn :: Prelude.Text,
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
    -- | The status of a security control based on the compliance status of its
    -- findings. For more information about how control status is determined,
    -- see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-overall-status.html Determining the overall status of a control from its findings>
    -- in the /Security Hub User Guide/.
    securityControlStatus :: ControlStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityControlId', 'securityControl_securityControlId' - The unique identifier of a security control across standards. Values for
-- this field typically consist of an Amazon Web Service name and a number,
-- such as APIGateway.3.
--
-- 'securityControlArn', 'securityControl_securityControlArn' - The Amazon Resource Name (ARN) for a security control across standards,
-- such as
-- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
-- This parameter doesn\'t mention a specific standard.
--
-- 'title', 'securityControl_title' - The title of a security control.
--
-- 'description', 'securityControl_description' - The description of a security control across standards. This typically
-- summarizes how Security Hub evaluates the control and the conditions
-- under which it produces a failed finding. This parameter doesn\'t
-- reference a specific standard.
--
-- 'remediationUrl', 'securityControl_remediationUrl' - A link to Security Hub documentation that explains how to remediate a
-- failed finding for a security control.
--
-- 'severityRating', 'securityControl_severityRating' - The severity of a security control. For more information about how
-- Security Hub determines control severity, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-findings-create-update.html#control-findings-severity Assigning severity to control findings>
-- in the /Security Hub User Guide/.
--
-- 'securityControlStatus', 'securityControl_securityControlStatus' - The status of a security control based on the compliance status of its
-- findings. For more information about how control status is determined,
-- see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-overall-status.html Determining the overall status of a control from its findings>
-- in the /Security Hub User Guide/.
newSecurityControl ::
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'securityControlArn'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'remediationUrl'
  Prelude.Text ->
  -- | 'severityRating'
  SeverityRating ->
  -- | 'securityControlStatus'
  ControlStatus ->
  SecurityControl
newSecurityControl
  pSecurityControlId_
  pSecurityControlArn_
  pTitle_
  pDescription_
  pRemediationUrl_
  pSeverityRating_
  pSecurityControlStatus_ =
    SecurityControl'
      { securityControlId =
          pSecurityControlId_,
        securityControlArn = pSecurityControlArn_,
        title = pTitle_,
        description = pDescription_,
        remediationUrl = pRemediationUrl_,
        severityRating = pSeverityRating_,
        securityControlStatus = pSecurityControlStatus_
      }

-- | The unique identifier of a security control across standards. Values for
-- this field typically consist of an Amazon Web Service name and a number,
-- such as APIGateway.3.
securityControl_securityControlId :: Lens.Lens' SecurityControl Prelude.Text
securityControl_securityControlId = Lens.lens (\SecurityControl' {securityControlId} -> securityControlId) (\s@SecurityControl' {} a -> s {securityControlId = a} :: SecurityControl)

-- | The Amazon Resource Name (ARN) for a security control across standards,
-- such as
-- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
-- This parameter doesn\'t mention a specific standard.
securityControl_securityControlArn :: Lens.Lens' SecurityControl Prelude.Text
securityControl_securityControlArn = Lens.lens (\SecurityControl' {securityControlArn} -> securityControlArn) (\s@SecurityControl' {} a -> s {securityControlArn = a} :: SecurityControl)

-- | The title of a security control.
securityControl_title :: Lens.Lens' SecurityControl Prelude.Text
securityControl_title = Lens.lens (\SecurityControl' {title} -> title) (\s@SecurityControl' {} a -> s {title = a} :: SecurityControl)

-- | The description of a security control across standards. This typically
-- summarizes how Security Hub evaluates the control and the conditions
-- under which it produces a failed finding. This parameter doesn\'t
-- reference a specific standard.
securityControl_description :: Lens.Lens' SecurityControl Prelude.Text
securityControl_description = Lens.lens (\SecurityControl' {description} -> description) (\s@SecurityControl' {} a -> s {description = a} :: SecurityControl)

-- | A link to Security Hub documentation that explains how to remediate a
-- failed finding for a security control.
securityControl_remediationUrl :: Lens.Lens' SecurityControl Prelude.Text
securityControl_remediationUrl = Lens.lens (\SecurityControl' {remediationUrl} -> remediationUrl) (\s@SecurityControl' {} a -> s {remediationUrl = a} :: SecurityControl)

-- | The severity of a security control. For more information about how
-- Security Hub determines control severity, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-findings-create-update.html#control-findings-severity Assigning severity to control findings>
-- in the /Security Hub User Guide/.
securityControl_severityRating :: Lens.Lens' SecurityControl SeverityRating
securityControl_severityRating = Lens.lens (\SecurityControl' {severityRating} -> severityRating) (\s@SecurityControl' {} a -> s {severityRating = a} :: SecurityControl)

-- | The status of a security control based on the compliance status of its
-- findings. For more information about how control status is determined,
-- see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/controls-overall-status.html Determining the overall status of a control from its findings>
-- in the /Security Hub User Guide/.
securityControl_securityControlStatus :: Lens.Lens' SecurityControl ControlStatus
securityControl_securityControlStatus = Lens.lens (\SecurityControl' {securityControlStatus} -> securityControlStatus) (\s@SecurityControl' {} a -> s {securityControlStatus = a} :: SecurityControl)

instance Data.FromJSON SecurityControl where
  parseJSON =
    Data.withObject
      "SecurityControl"
      ( \x ->
          SecurityControl'
            Prelude.<$> (x Data..: "SecurityControlId")
            Prelude.<*> (x Data..: "SecurityControlArn")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "RemediationUrl")
            Prelude.<*> (x Data..: "SeverityRating")
            Prelude.<*> (x Data..: "SecurityControlStatus")
      )

instance Prelude.Hashable SecurityControl where
  hashWithSalt _salt SecurityControl' {..} =
    _salt
      `Prelude.hashWithSalt` securityControlId
      `Prelude.hashWithSalt` securityControlArn
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` remediationUrl
      `Prelude.hashWithSalt` severityRating
      `Prelude.hashWithSalt` securityControlStatus

instance Prelude.NFData SecurityControl where
  rnf SecurityControl' {..} =
    Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf securityControlArn
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf remediationUrl
      `Prelude.seq` Prelude.rnf severityRating
      `Prelude.seq` Prelude.rnf securityControlStatus
