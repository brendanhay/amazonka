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
-- Module      : Network.AWS.IoT.Types.DetectMitigationActionsTaskTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DetectMitigationActionsTaskTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The target of a mitigation action task.
--
-- /See:/ 'newDetectMitigationActionsTaskTarget' smart constructor.
data DetectMitigationActionsTaskTarget = DetectMitigationActionsTaskTarget'
  { -- | The unique identifiers of the violations.
    violationIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The name of the behavior.
    behaviorName :: Core.Maybe Core.Text,
    -- | The name of the security profile.
    securityProfileName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectMitigationActionsTaskTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationIds', 'detectMitigationActionsTaskTarget_violationIds' - The unique identifiers of the violations.
--
-- 'behaviorName', 'detectMitigationActionsTaskTarget_behaviorName' - The name of the behavior.
--
-- 'securityProfileName', 'detectMitigationActionsTaskTarget_securityProfileName' - The name of the security profile.
newDetectMitigationActionsTaskTarget ::
  DetectMitigationActionsTaskTarget
newDetectMitigationActionsTaskTarget =
  DetectMitigationActionsTaskTarget'
    { violationIds =
        Core.Nothing,
      behaviorName = Core.Nothing,
      securityProfileName = Core.Nothing
    }

-- | The unique identifiers of the violations.
detectMitigationActionsTaskTarget_violationIds :: Lens.Lens' DetectMitigationActionsTaskTarget (Core.Maybe (Core.NonEmpty Core.Text))
detectMitigationActionsTaskTarget_violationIds = Lens.lens (\DetectMitigationActionsTaskTarget' {violationIds} -> violationIds) (\s@DetectMitigationActionsTaskTarget' {} a -> s {violationIds = a} :: DetectMitigationActionsTaskTarget) Core.. Lens.mapping Lens._Coerce

-- | The name of the behavior.
detectMitigationActionsTaskTarget_behaviorName :: Lens.Lens' DetectMitigationActionsTaskTarget (Core.Maybe Core.Text)
detectMitigationActionsTaskTarget_behaviorName = Lens.lens (\DetectMitigationActionsTaskTarget' {behaviorName} -> behaviorName) (\s@DetectMitigationActionsTaskTarget' {} a -> s {behaviorName = a} :: DetectMitigationActionsTaskTarget)

-- | The name of the security profile.
detectMitigationActionsTaskTarget_securityProfileName :: Lens.Lens' DetectMitigationActionsTaskTarget (Core.Maybe Core.Text)
detectMitigationActionsTaskTarget_securityProfileName = Lens.lens (\DetectMitigationActionsTaskTarget' {securityProfileName} -> securityProfileName) (\s@DetectMitigationActionsTaskTarget' {} a -> s {securityProfileName = a} :: DetectMitigationActionsTaskTarget)

instance
  Core.FromJSON
    DetectMitigationActionsTaskTarget
  where
  parseJSON =
    Core.withObject
      "DetectMitigationActionsTaskTarget"
      ( \x ->
          DetectMitigationActionsTaskTarget'
            Core.<$> (x Core..:? "violationIds")
            Core.<*> (x Core..:? "behaviorName")
            Core.<*> (x Core..:? "securityProfileName")
      )

instance
  Core.Hashable
    DetectMitigationActionsTaskTarget

instance
  Core.NFData
    DetectMitigationActionsTaskTarget

instance
  Core.ToJSON
    DetectMitigationActionsTaskTarget
  where
  toJSON DetectMitigationActionsTaskTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("violationIds" Core..=) Core.<$> violationIds,
            ("behaviorName" Core..=) Core.<$> behaviorName,
            ("securityProfileName" Core..=)
              Core.<$> securityProfileName
          ]
      )
