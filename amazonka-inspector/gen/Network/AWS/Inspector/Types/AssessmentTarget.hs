{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Inspector.Types.AssessmentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTarget where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an Amazon Inspector application. This data
-- type is used as the response element in the DescribeAssessmentTargets
-- action.
--
-- /See:/ 'newAssessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { -- | The ARN that specifies the resource group that is associated with the
    -- assessment target.
    resourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN that specifies the Amazon Inspector assessment target.
    arn :: Prelude.Text,
    -- | The name of the Amazon Inspector assessment target.
    name :: Prelude.Text,
    -- | The time at which the assessment target is created.
    createdAt :: Prelude.POSIX,
    -- | The time at which UpdateAssessmentTarget is called.
    updatedAt :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssessmentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupArn', 'assessmentTarget_resourceGroupArn' - The ARN that specifies the resource group that is associated with the
-- assessment target.
--
-- 'arn', 'assessmentTarget_arn' - The ARN that specifies the Amazon Inspector assessment target.
--
-- 'name', 'assessmentTarget_name' - The name of the Amazon Inspector assessment target.
--
-- 'createdAt', 'assessmentTarget_createdAt' - The time at which the assessment target is created.
--
-- 'updatedAt', 'assessmentTarget_updatedAt' - The time at which UpdateAssessmentTarget is called.
newAssessmentTarget ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  AssessmentTarget
newAssessmentTarget
  pArn_
  pName_
  pCreatedAt_
  pUpdatedAt_ =
    AssessmentTarget'
      { resourceGroupArn =
          Prelude.Nothing,
        arn = pArn_,
        name = pName_,
        createdAt = Prelude._Time Lens.# pCreatedAt_,
        updatedAt = Prelude._Time Lens.# pUpdatedAt_
      }

-- | The ARN that specifies the resource group that is associated with the
-- assessment target.
assessmentTarget_resourceGroupArn :: Lens.Lens' AssessmentTarget (Prelude.Maybe Prelude.Text)
assessmentTarget_resourceGroupArn = Lens.lens (\AssessmentTarget' {resourceGroupArn} -> resourceGroupArn) (\s@AssessmentTarget' {} a -> s {resourceGroupArn = a} :: AssessmentTarget)

-- | The ARN that specifies the Amazon Inspector assessment target.
assessmentTarget_arn :: Lens.Lens' AssessmentTarget Prelude.Text
assessmentTarget_arn = Lens.lens (\AssessmentTarget' {arn} -> arn) (\s@AssessmentTarget' {} a -> s {arn = a} :: AssessmentTarget)

-- | The name of the Amazon Inspector assessment target.
assessmentTarget_name :: Lens.Lens' AssessmentTarget Prelude.Text
assessmentTarget_name = Lens.lens (\AssessmentTarget' {name} -> name) (\s@AssessmentTarget' {} a -> s {name = a} :: AssessmentTarget)

-- | The time at which the assessment target is created.
assessmentTarget_createdAt :: Lens.Lens' AssessmentTarget Prelude.UTCTime
assessmentTarget_createdAt = Lens.lens (\AssessmentTarget' {createdAt} -> createdAt) (\s@AssessmentTarget' {} a -> s {createdAt = a} :: AssessmentTarget) Prelude.. Prelude._Time

-- | The time at which UpdateAssessmentTarget is called.
assessmentTarget_updatedAt :: Lens.Lens' AssessmentTarget Prelude.UTCTime
assessmentTarget_updatedAt = Lens.lens (\AssessmentTarget' {updatedAt} -> updatedAt) (\s@AssessmentTarget' {} a -> s {updatedAt = a} :: AssessmentTarget) Prelude.. Prelude._Time

instance Prelude.FromJSON AssessmentTarget where
  parseJSON =
    Prelude.withObject
      "AssessmentTarget"
      ( \x ->
          AssessmentTarget'
            Prelude.<$> (x Prelude..:? "resourceGroupArn")
            Prelude.<*> (x Prelude..: "arn")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "createdAt")
            Prelude.<*> (x Prelude..: "updatedAt")
      )

instance Prelude.Hashable AssessmentTarget

instance Prelude.NFData AssessmentTarget
