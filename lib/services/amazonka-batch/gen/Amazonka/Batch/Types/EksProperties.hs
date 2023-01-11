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
-- Module      : Amazonka.Batch.Types.EksProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksProperties where

import Amazonka.Batch.Types.EksPodProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the properties for the Kubernetes resources of a
-- job.
--
-- /See:/ 'newEksProperties' smart constructor.
data EksProperties = EksProperties'
  { -- | The properties for the Kubernetes pod resources of a job.
    podProperties :: Prelude.Maybe EksPodProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'podProperties', 'eksProperties_podProperties' - The properties for the Kubernetes pod resources of a job.
newEksProperties ::
  EksProperties
newEksProperties =
  EksProperties' {podProperties = Prelude.Nothing}

-- | The properties for the Kubernetes pod resources of a job.
eksProperties_podProperties :: Lens.Lens' EksProperties (Prelude.Maybe EksPodProperties)
eksProperties_podProperties = Lens.lens (\EksProperties' {podProperties} -> podProperties) (\s@EksProperties' {} a -> s {podProperties = a} :: EksProperties)

instance Data.FromJSON EksProperties where
  parseJSON =
    Data.withObject
      "EksProperties"
      ( \x ->
          EksProperties'
            Prelude.<$> (x Data..:? "podProperties")
      )

instance Prelude.Hashable EksProperties where
  hashWithSalt _salt EksProperties' {..} =
    _salt `Prelude.hashWithSalt` podProperties

instance Prelude.NFData EksProperties where
  rnf EksProperties' {..} = Prelude.rnf podProperties

instance Data.ToJSON EksProperties where
  toJSON EksProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("podProperties" Data..=)
              Prelude.<$> podProperties
          ]
      )
