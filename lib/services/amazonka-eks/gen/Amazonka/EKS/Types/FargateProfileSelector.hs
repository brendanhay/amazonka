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
-- Module      : Amazonka.EKS.Types.FargateProfileSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.FargateProfileSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Fargate profile selector.
--
-- /See:/ 'newFargateProfileSelector' smart constructor.
data FargateProfileSelector = FargateProfileSelector'
  { -- | The Kubernetes labels that the selector should match. A pod must contain
    -- all of the labels that are specified in the selector for it to be
    -- considered a match.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Kubernetes namespace that the selector should match.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { labels = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The Kubernetes labels that the selector should match. A pod must contain
-- all of the labels that are specified in the selector for it to be
-- considered a match.
fargateProfileSelector_labels :: Lens.Lens' FargateProfileSelector (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
fargateProfileSelector_labels = Lens.lens (\FargateProfileSelector' {labels} -> labels) (\s@FargateProfileSelector' {} a -> s {labels = a} :: FargateProfileSelector) Prelude.. Lens.mapping Lens.coerced

-- | The Kubernetes namespace that the selector should match.
fargateProfileSelector_namespace :: Lens.Lens' FargateProfileSelector (Prelude.Maybe Prelude.Text)
fargateProfileSelector_namespace = Lens.lens (\FargateProfileSelector' {namespace} -> namespace) (\s@FargateProfileSelector' {} a -> s {namespace = a} :: FargateProfileSelector)

instance Data.FromJSON FargateProfileSelector where
  parseJSON =
    Data.withObject
      "FargateProfileSelector"
      ( \x ->
          FargateProfileSelector'
            Prelude.<$> (x Data..:? "labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "namespace")
      )

instance Prelude.Hashable FargateProfileSelector where
  hashWithSalt _salt FargateProfileSelector' {..} =
    _salt
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData FargateProfileSelector where
  rnf FargateProfileSelector' {..} =
    Prelude.rnf labels `Prelude.seq`
      Prelude.rnf namespace

instance Data.ToJSON FargateProfileSelector where
  toJSON FargateProfileSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("labels" Data..=) Prelude.<$> labels,
            ("namespace" Data..=) Prelude.<$> namespace
          ]
      )
