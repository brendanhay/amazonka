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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalyResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalyResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Web Services resources in which DevOps Guru detected unusual
-- behavior that resulted in the generation of an anomaly. When DevOps Guru
-- detects multiple related anomalies, it creates and insight with details
-- about the anomalous behavior and suggestions about how to correct the
-- problem.
--
-- /See:/ 'newAnomalyResource' smart constructor.
data AnomalyResource = AnomalyResource'
  { -- | The name of the Amazon Web Services resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the Amazon Web Services resource.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'anomalyResource_name' - The name of the Amazon Web Services resource.
--
-- 'type'', 'anomalyResource_type' - The type of the Amazon Web Services resource.
newAnomalyResource ::
  AnomalyResource
newAnomalyResource =
  AnomalyResource'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the Amazon Web Services resource.
anomalyResource_name :: Lens.Lens' AnomalyResource (Prelude.Maybe Prelude.Text)
anomalyResource_name = Lens.lens (\AnomalyResource' {name} -> name) (\s@AnomalyResource' {} a -> s {name = a} :: AnomalyResource)

-- | The type of the Amazon Web Services resource.
anomalyResource_type :: Lens.Lens' AnomalyResource (Prelude.Maybe Prelude.Text)
anomalyResource_type = Lens.lens (\AnomalyResource' {type'} -> type') (\s@AnomalyResource' {} a -> s {type' = a} :: AnomalyResource)

instance Data.FromJSON AnomalyResource where
  parseJSON =
    Data.withObject
      "AnomalyResource"
      ( \x ->
          AnomalyResource'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AnomalyResource where
  hashWithSalt _salt AnomalyResource' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AnomalyResource where
  rnf AnomalyResource' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
