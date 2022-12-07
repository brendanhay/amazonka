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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.LifecyclePolicyPreviewResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.LifecyclePolicyRuleAction
import qualified Amazonka.Prelude as Prelude

-- | The result of the lifecycle policy preview.
--
-- /See:/ 'newLifecyclePolicyPreviewResult' smart constructor.
data LifecyclePolicyPreviewResult = LifecyclePolicyPreviewResult'
  { -- | The priority of the applied rule.
    appliedRulePriority :: Prelude.Maybe Prelude.Natural,
    -- | The date and time, expressed in standard JavaScript date format, at
    -- which the current image was pushed to the repository.
    imagePushedAt :: Prelude.Maybe Data.POSIX,
    -- | The list of tags associated with this image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The type of action to be taken.
    action :: Prelude.Maybe LifecyclePolicyRuleAction,
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text
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
-- 'appliedRulePriority', 'lifecyclePolicyPreviewResult_appliedRulePriority' - The priority of the applied rule.
--
-- 'imagePushedAt', 'lifecyclePolicyPreviewResult_imagePushedAt' - The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
--
-- 'imageTags', 'lifecyclePolicyPreviewResult_imageTags' - The list of tags associated with this image.
--
-- 'action', 'lifecyclePolicyPreviewResult_action' - The type of action to be taken.
--
-- 'imageDigest', 'lifecyclePolicyPreviewResult_imageDigest' - The @sha256@ digest of the image manifest.
newLifecyclePolicyPreviewResult ::
  LifecyclePolicyPreviewResult
newLifecyclePolicyPreviewResult =
  LifecyclePolicyPreviewResult'
    { appliedRulePriority =
        Prelude.Nothing,
      imagePushedAt = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      action = Prelude.Nothing,
      imageDigest = Prelude.Nothing
    }

-- | The priority of the applied rule.
lifecyclePolicyPreviewResult_appliedRulePriority :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe Prelude.Natural)
lifecyclePolicyPreviewResult_appliedRulePriority = Lens.lens (\LifecyclePolicyPreviewResult' {appliedRulePriority} -> appliedRulePriority) (\s@LifecyclePolicyPreviewResult' {} a -> s {appliedRulePriority = a} :: LifecyclePolicyPreviewResult)

-- | The date and time, expressed in standard JavaScript date format, at
-- which the current image was pushed to the repository.
lifecyclePolicyPreviewResult_imagePushedAt :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe Prelude.UTCTime)
lifecyclePolicyPreviewResult_imagePushedAt = Lens.lens (\LifecyclePolicyPreviewResult' {imagePushedAt} -> imagePushedAt) (\s@LifecyclePolicyPreviewResult' {} a -> s {imagePushedAt = a} :: LifecyclePolicyPreviewResult) Prelude.. Lens.mapping Data._Time

-- | The list of tags associated with this image.
lifecyclePolicyPreviewResult_imageTags :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe [Prelude.Text])
lifecyclePolicyPreviewResult_imageTags = Lens.lens (\LifecyclePolicyPreviewResult' {imageTags} -> imageTags) (\s@LifecyclePolicyPreviewResult' {} a -> s {imageTags = a} :: LifecyclePolicyPreviewResult) Prelude.. Lens.mapping Lens.coerced

-- | The type of action to be taken.
lifecyclePolicyPreviewResult_action :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe LifecyclePolicyRuleAction)
lifecyclePolicyPreviewResult_action = Lens.lens (\LifecyclePolicyPreviewResult' {action} -> action) (\s@LifecyclePolicyPreviewResult' {} a -> s {action = a} :: LifecyclePolicyPreviewResult)

-- | The @sha256@ digest of the image manifest.
lifecyclePolicyPreviewResult_imageDigest :: Lens.Lens' LifecyclePolicyPreviewResult (Prelude.Maybe Prelude.Text)
lifecyclePolicyPreviewResult_imageDigest = Lens.lens (\LifecyclePolicyPreviewResult' {imageDigest} -> imageDigest) (\s@LifecyclePolicyPreviewResult' {} a -> s {imageDigest = a} :: LifecyclePolicyPreviewResult)

instance Data.FromJSON LifecyclePolicyPreviewResult where
  parseJSON =
    Data.withObject
      "LifecyclePolicyPreviewResult"
      ( \x ->
          LifecyclePolicyPreviewResult'
            Prelude.<$> (x Data..:? "appliedRulePriority")
            Prelude.<*> (x Data..:? "imagePushedAt")
            Prelude.<*> (x Data..:? "imageTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "action")
            Prelude.<*> (x Data..:? "imageDigest")
      )

instance
  Prelude.Hashable
    LifecyclePolicyPreviewResult
  where
  hashWithSalt _salt LifecyclePolicyPreviewResult' {..} =
    _salt `Prelude.hashWithSalt` appliedRulePriority
      `Prelude.hashWithSalt` imagePushedAt
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` imageDigest

instance Prelude.NFData LifecyclePolicyPreviewResult where
  rnf LifecyclePolicyPreviewResult' {..} =
    Prelude.rnf appliedRulePriority
      `Prelude.seq` Prelude.rnf imagePushedAt
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf imageDigest
