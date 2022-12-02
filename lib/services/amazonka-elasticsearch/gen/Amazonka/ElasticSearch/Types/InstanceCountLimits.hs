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
-- Module      : Amazonka.ElasticSearch.Types.InstanceCountLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.InstanceCountLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | InstanceCountLimits represents the limits on number of instances that be
-- created in Amazon Elasticsearch for given InstanceType.
--
-- /See:/ 'newInstanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { minimumInstanceCount :: Prelude.Maybe Prelude.Int,
    maximumInstanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumInstanceCount', 'instanceCountLimits_minimumInstanceCount' - Undocumented member.
--
-- 'maximumInstanceCount', 'instanceCountLimits_maximumInstanceCount' - Undocumented member.
newInstanceCountLimits ::
  InstanceCountLimits
newInstanceCountLimits =
  InstanceCountLimits'
    { minimumInstanceCount =
        Prelude.Nothing,
      maximumInstanceCount = Prelude.Nothing
    }

-- | Undocumented member.
instanceCountLimits_minimumInstanceCount :: Lens.Lens' InstanceCountLimits (Prelude.Maybe Prelude.Int)
instanceCountLimits_minimumInstanceCount = Lens.lens (\InstanceCountLimits' {minimumInstanceCount} -> minimumInstanceCount) (\s@InstanceCountLimits' {} a -> s {minimumInstanceCount = a} :: InstanceCountLimits)

-- | Undocumented member.
instanceCountLimits_maximumInstanceCount :: Lens.Lens' InstanceCountLimits (Prelude.Maybe Prelude.Int)
instanceCountLimits_maximumInstanceCount = Lens.lens (\InstanceCountLimits' {maximumInstanceCount} -> maximumInstanceCount) (\s@InstanceCountLimits' {} a -> s {maximumInstanceCount = a} :: InstanceCountLimits)

instance Data.FromJSON InstanceCountLimits where
  parseJSON =
    Data.withObject
      "InstanceCountLimits"
      ( \x ->
          InstanceCountLimits'
            Prelude.<$> (x Data..:? "MinimumInstanceCount")
            Prelude.<*> (x Data..:? "MaximumInstanceCount")
      )

instance Prelude.Hashable InstanceCountLimits where
  hashWithSalt _salt InstanceCountLimits' {..} =
    _salt `Prelude.hashWithSalt` minimumInstanceCount
      `Prelude.hashWithSalt` maximumInstanceCount

instance Prelude.NFData InstanceCountLimits where
  rnf InstanceCountLimits' {..} =
    Prelude.rnf minimumInstanceCount
      `Prelude.seq` Prelude.rnf maximumInstanceCount
