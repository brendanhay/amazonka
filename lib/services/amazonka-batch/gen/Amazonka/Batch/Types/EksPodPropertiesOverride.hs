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
-- Module      : Amazonka.Batch.Types.EksPodPropertiesOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksPodPropertiesOverride where

import Amazonka.Batch.Types.EksContainerOverride
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains overrides for the Kubernetes pod properties of a
-- job.
--
-- /See:/ 'newEksPodPropertiesOverride' smart constructor.
data EksPodPropertiesOverride = EksPodPropertiesOverride'
  { -- | The overrides for the container that\'s used on the Amazon EKS pod.
    containers :: Prelude.Maybe [EksContainerOverride]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksPodPropertiesOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'eksPodPropertiesOverride_containers' - The overrides for the container that\'s used on the Amazon EKS pod.
newEksPodPropertiesOverride ::
  EksPodPropertiesOverride
newEksPodPropertiesOverride =
  EksPodPropertiesOverride'
    { containers =
        Prelude.Nothing
    }

-- | The overrides for the container that\'s used on the Amazon EKS pod.
eksPodPropertiesOverride_containers :: Lens.Lens' EksPodPropertiesOverride (Prelude.Maybe [EksContainerOverride])
eksPodPropertiesOverride_containers = Lens.lens (\EksPodPropertiesOverride' {containers} -> containers) (\s@EksPodPropertiesOverride' {} a -> s {containers = a} :: EksPodPropertiesOverride) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EksPodPropertiesOverride where
  hashWithSalt _salt EksPodPropertiesOverride' {..} =
    _salt `Prelude.hashWithSalt` containers

instance Prelude.NFData EksPodPropertiesOverride where
  rnf EksPodPropertiesOverride' {..} =
    Prelude.rnf containers

instance Data.ToJSON EksPodPropertiesOverride where
  toJSON EksPodPropertiesOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [("containers" Data..=) Prelude.<$> containers]
      )
