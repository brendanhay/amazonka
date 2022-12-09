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
-- Module      : Amazonka.OpenSearchServerless.Types.SecurityPolicyStats
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SecurityPolicyStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics for an OpenSearch Serverless security policy.
--
-- /See:/ 'newSecurityPolicyStats' smart constructor.
data SecurityPolicyStats = SecurityPolicyStats'
  { -- | The number of encryption policies in the current account.
    encryptionPolicyCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of network policies in the current account.
    networkPolicyCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityPolicyStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionPolicyCount', 'securityPolicyStats_encryptionPolicyCount' - The number of encryption policies in the current account.
--
-- 'networkPolicyCount', 'securityPolicyStats_networkPolicyCount' - The number of network policies in the current account.
newSecurityPolicyStats ::
  SecurityPolicyStats
newSecurityPolicyStats =
  SecurityPolicyStats'
    { encryptionPolicyCount =
        Prelude.Nothing,
      networkPolicyCount = Prelude.Nothing
    }

-- | The number of encryption policies in the current account.
securityPolicyStats_encryptionPolicyCount :: Lens.Lens' SecurityPolicyStats (Prelude.Maybe Prelude.Integer)
securityPolicyStats_encryptionPolicyCount = Lens.lens (\SecurityPolicyStats' {encryptionPolicyCount} -> encryptionPolicyCount) (\s@SecurityPolicyStats' {} a -> s {encryptionPolicyCount = a} :: SecurityPolicyStats)

-- | The number of network policies in the current account.
securityPolicyStats_networkPolicyCount :: Lens.Lens' SecurityPolicyStats (Prelude.Maybe Prelude.Integer)
securityPolicyStats_networkPolicyCount = Lens.lens (\SecurityPolicyStats' {networkPolicyCount} -> networkPolicyCount) (\s@SecurityPolicyStats' {} a -> s {networkPolicyCount = a} :: SecurityPolicyStats)

instance Data.FromJSON SecurityPolicyStats where
  parseJSON =
    Data.withObject
      "SecurityPolicyStats"
      ( \x ->
          SecurityPolicyStats'
            Prelude.<$> (x Data..:? "EncryptionPolicyCount")
            Prelude.<*> (x Data..:? "NetworkPolicyCount")
      )

instance Prelude.Hashable SecurityPolicyStats where
  hashWithSalt _salt SecurityPolicyStats' {..} =
    _salt `Prelude.hashWithSalt` encryptionPolicyCount
      `Prelude.hashWithSalt` networkPolicyCount

instance Prelude.NFData SecurityPolicyStats where
  rnf SecurityPolicyStats' {..} =
    Prelude.rnf encryptionPolicyCount
      `Prelude.seq` Prelude.rnf networkPolicyCount
