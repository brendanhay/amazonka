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
-- Module      : Network.AWS.EKS.Types.FargateProfileSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.FargateProfileSelector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an AWS Fargate profile selector.
--
-- /See:/ 'newFargateProfileSelector' smart constructor.
data FargateProfileSelector = FargateProfileSelector'
  { -- | The Kubernetes labels that the selector should match. A pod must contain
    -- all of the labels that are specified in the selector for it to be
    -- considered a match.
    labels :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Kubernetes namespace that the selector should match.
    namespace :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FargateProfileSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'fargateProfileSelector_labels' - The Kubernetes labels that the selector should match. A pod must contain
-- all of the labels that are specified in the selector for it to be
-- considered a match.
--
-- 'namespace', 'fargateProfileSelector_namespace' - The Kubernetes namespace that the selector should match.
newFargateProfileSelector ::
  FargateProfileSelector
newFargateProfileSelector =
  FargateProfileSelector'
    { labels = Core.Nothing,
      namespace = Core.Nothing
    }

-- | The Kubernetes labels that the selector should match. A pod must contain
-- all of the labels that are specified in the selector for it to be
-- considered a match.
fargateProfileSelector_labels :: Lens.Lens' FargateProfileSelector (Core.Maybe (Core.HashMap Core.Text Core.Text))
fargateProfileSelector_labels = Lens.lens (\FargateProfileSelector' {labels} -> labels) (\s@FargateProfileSelector' {} a -> s {labels = a} :: FargateProfileSelector) Core.. Lens.mapping Lens._Coerce

-- | The Kubernetes namespace that the selector should match.
fargateProfileSelector_namespace :: Lens.Lens' FargateProfileSelector (Core.Maybe Core.Text)
fargateProfileSelector_namespace = Lens.lens (\FargateProfileSelector' {namespace} -> namespace) (\s@FargateProfileSelector' {} a -> s {namespace = a} :: FargateProfileSelector)

instance Core.FromJSON FargateProfileSelector where
  parseJSON =
    Core.withObject
      "FargateProfileSelector"
      ( \x ->
          FargateProfileSelector'
            Core.<$> (x Core..:? "labels" Core..!= Core.mempty)
            Core.<*> (x Core..:? "namespace")
      )

instance Core.Hashable FargateProfileSelector

instance Core.NFData FargateProfileSelector

instance Core.ToJSON FargateProfileSelector where
  toJSON FargateProfileSelector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("labels" Core..=) Core.<$> labels,
            ("namespace" Core..=) Core.<$> namespace
          ]
      )
