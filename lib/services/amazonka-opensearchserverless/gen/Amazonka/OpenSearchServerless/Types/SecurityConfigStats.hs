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
-- Module      : Amazonka.OpenSearchServerless.Types.SecurityConfigStats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SecurityConfigStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics for an OpenSearch Serverless security configuration.
--
-- /See:/ 'newSecurityConfigStats' smart constructor.
data SecurityConfigStats = SecurityConfigStats'
  { -- | The number of security configurations in the current account.
    samlConfigCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfigStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samlConfigCount', 'securityConfigStats_samlConfigCount' - The number of security configurations in the current account.
newSecurityConfigStats ::
  SecurityConfigStats
newSecurityConfigStats =
  SecurityConfigStats'
    { samlConfigCount =
        Prelude.Nothing
    }

-- | The number of security configurations in the current account.
securityConfigStats_samlConfigCount :: Lens.Lens' SecurityConfigStats (Prelude.Maybe Prelude.Integer)
securityConfigStats_samlConfigCount = Lens.lens (\SecurityConfigStats' {samlConfigCount} -> samlConfigCount) (\s@SecurityConfigStats' {} a -> s {samlConfigCount = a} :: SecurityConfigStats)

instance Data.FromJSON SecurityConfigStats where
  parseJSON =
    Data.withObject
      "SecurityConfigStats"
      ( \x ->
          SecurityConfigStats'
            Prelude.<$> (x Data..:? "SamlConfigCount")
      )

instance Prelude.Hashable SecurityConfigStats where
  hashWithSalt _salt SecurityConfigStats' {..} =
    _salt `Prelude.hashWithSalt` samlConfigCount

instance Prelude.NFData SecurityConfigStats where
  rnf SecurityConfigStats' {..} =
    Prelude.rnf samlConfigCount
