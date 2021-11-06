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
-- Module      : Amazonka.ECR.Types.LifecyclePolicyPreviewResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.LifecyclePolicyPreviewResult where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types.LifecyclePolicyRuleAction
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The result of the lifecycle policy preview.
--
-- /See:/ 'newLifecyclePolicyPreviewResult' smart constructor.
data LifecyclePolicyPreviewResult = LifecyclePolicyPreviewResult'
  { -- | The list of tags associated with this image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The type of action to be taken.
    action :: Prelude.Maybe LifecyclePolicyRuleAction,
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The date and time, expressed in standard JavaScript date format, at
    -- which the current image was pushed to the repository.
    imagePushedAt :: Prelude.Maybe Core.POSIX,
    -- | The priority of the applied rule.
    appliedRulePriority :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecyclePolicyPreviewResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageTags', 'lifecyclePolicyPreviewResult_imageTags' - The list of tags associated with this image.
--
-- 'action', 'lifecyclePolicyPreviewResult_action' - The type of action to be taken.
--
-- 'imageDigest', 'lifecyclePolicyPreviewResult_imageDigest' - The @sha256@ digest of the image manifest.
--
-- 'imagePushedAt', 'lifecyclePolicyPreviewResult_imagePushedAt' - The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
--
-- 'appliedRulePriority', 'lifecyclePolicyPreviewResult_appliedRulePriority' - The priority of the applied rule.
newLifecyclePolicyPreviewResult ::
  LifecyclePolicyPreviewResult
newLifecyclePolicyPreviewResult =
  LifecyclePolicyPreviewResult'
    { imageTags =
        Prelude.Nothing,
      action = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      imagePushedAt = Prelude.Nothing,
      appliedRulePriority = Prelude.Nothing
    }

-- | The list of tags associated with this image.
lifecyclePolicyPreviewResult_imageTags :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe [Prelude.Text])
lifecyclePolicyPreviewResult_imageTags = Lens.lens (\LifecyclePolicyPreviewResult' {imageTags} -> imageTags) (\s@LifecyclePolicyPreviewResult' {} a -> s {imageTags = a} :: LifecyclePolicyPreviewResult) Prelude.. Lens.mapping Lens.coerced

-- | The type of action to be taken.
lifecyclePolicyPreviewResult_action :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe LifecyclePolicyRuleAction)
lifecyclePolicyPreviewResult_action = Lens.lens (\LifecyclePolicyPreviewResult' {action} -> action) (\s@LifecyclePolicyPreviewResult' {} a -> s {action = a} :: LifecyclePolicyPreviewResult)

-- | The @sha256@ digest of the image manifest.
lifecyclePolicyPreviewResult_imageDigest :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe Prelude.Text)
lifecyclePolicyPreviewResult_imageDigest = Lens.lens (\LifecyclePolicyPreviewResult' {imageDigest} -> imageDigest) (\s@LifecyclePolicyPreviewResult' {} a -> s {imageDigest = a} :: LifecyclePolicyPreviewResult)

-- | The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
lifecyclePolicyPreviewResult_imagePushedAt :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe Prelude.UTCTime)
lifecyclePolicyPreviewResult_imagePushedAt = Lens.lens (\LifecyclePolicyPreviewResult' {imagePushedAt} -> imagePushedAt) (\s@LifecyclePolicyPreviewResult' {} a -> s {imagePushedAt = a} :: LifecyclePolicyPreviewResult) Prelude.. Lens.mapping Core._Time

-- | The priority of the applied rule.
lifecyclePolicyPreviewResult_appliedRulePriority :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe Prelude.Natural)
lifecyclePolicyPreviewResult_appliedRulePriority = Lens.lens (\LifecyclePolicyPreviewResult' {appliedRulePriority} -> appliedRulePriority) (\s@LifecyclePolicyPreviewResult' {} a -> s {appliedRulePriority = a} :: LifecyclePolicyPreviewResult)

instance Core.FromJSON LifecyclePolicyPreviewResult where
  parseJSON =
    Core.withObject
      "LifecyclePolicyPreviewResult"
      ( \x ->
          LifecyclePolicyPreviewResult'
            Prelude.<$> (x Core..:? "imageTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "action")
            Prelude.<*> (x Core..:? "imageDigest")
            Prelude.<*> (x Core..:? "imagePushedAt")
            Prelude.<*> (x Core..:? "appliedRulePriority")
      )

instance
  Prelude.Hashable
    LifecyclePolicyPreviewResult

instance Prelude.NFData LifecyclePolicyPreviewResult
