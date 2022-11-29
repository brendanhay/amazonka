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
-- Module      : Amazonka.Batch.Types.EksPropertiesOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksPropertiesOverride where

import Amazonka.Batch.Types.EksPodPropertiesOverride
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains overrides for the Kubernetes resources of a job.
--
-- /See:/ 'newEksPropertiesOverride' smart constructor.
data EksPropertiesOverride = EksPropertiesOverride'
  { -- | The overrides for the Kubernetes pod resources of a job.
    podProperties :: Prelude.Maybe EksPodPropertiesOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksPropertiesOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'podProperties', 'eksPropertiesOverride_podProperties' - The overrides for the Kubernetes pod resources of a job.
newEksPropertiesOverride ::
  EksPropertiesOverride
newEksPropertiesOverride =
  EksPropertiesOverride'
    { podProperties =
        Prelude.Nothing
    }

-- | The overrides for the Kubernetes pod resources of a job.
eksPropertiesOverride_podProperties :: Lens.Lens' EksPropertiesOverride (Prelude.Maybe EksPodPropertiesOverride)
eksPropertiesOverride_podProperties = Lens.lens (\EksPropertiesOverride' {podProperties} -> podProperties) (\s@EksPropertiesOverride' {} a -> s {podProperties = a} :: EksPropertiesOverride)

instance Prelude.Hashable EksPropertiesOverride where
  hashWithSalt _salt EksPropertiesOverride' {..} =
    _salt `Prelude.hashWithSalt` podProperties

instance Prelude.NFData EksPropertiesOverride where
  rnf EksPropertiesOverride' {..} =
    Prelude.rnf podProperties

instance Core.ToJSON EksPropertiesOverride where
  toJSON EksPropertiesOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("podProperties" Core..=)
              Prelude.<$> podProperties
          ]
      )
