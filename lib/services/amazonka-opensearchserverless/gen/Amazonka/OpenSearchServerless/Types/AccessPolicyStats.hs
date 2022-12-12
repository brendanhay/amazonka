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
-- Module      : Amazonka.OpenSearchServerless.Types.AccessPolicyStats
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.AccessPolicyStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics for an OpenSearch Serverless access policy.
--
-- /See:/ 'newAccessPolicyStats' smart constructor.
data AccessPolicyStats = AccessPolicyStats'
  { -- | The number of data access policies in the current account.
    dataPolicyCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPolicyStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPolicyCount', 'accessPolicyStats_dataPolicyCount' - The number of data access policies in the current account.
newAccessPolicyStats ::
  AccessPolicyStats
newAccessPolicyStats =
  AccessPolicyStats'
    { dataPolicyCount =
        Prelude.Nothing
    }

-- | The number of data access policies in the current account.
accessPolicyStats_dataPolicyCount :: Lens.Lens' AccessPolicyStats (Prelude.Maybe Prelude.Integer)
accessPolicyStats_dataPolicyCount = Lens.lens (\AccessPolicyStats' {dataPolicyCount} -> dataPolicyCount) (\s@AccessPolicyStats' {} a -> s {dataPolicyCount = a} :: AccessPolicyStats)

instance Data.FromJSON AccessPolicyStats where
  parseJSON =
    Data.withObject
      "AccessPolicyStats"
      ( \x ->
          AccessPolicyStats'
            Prelude.<$> (x Data..:? "DataPolicyCount")
      )

instance Prelude.Hashable AccessPolicyStats where
  hashWithSalt _salt AccessPolicyStats' {..} =
    _salt `Prelude.hashWithSalt` dataPolicyCount

instance Prelude.NFData AccessPolicyStats where
  rnf AccessPolicyStats' {..} =
    Prelude.rnf dataPolicyCount
