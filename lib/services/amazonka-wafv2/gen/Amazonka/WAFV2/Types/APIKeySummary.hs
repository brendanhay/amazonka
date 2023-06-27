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
-- Module      : Amazonka.WAFV2.Types.APIKeySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.APIKeySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information for a single API key.
--
-- API keys are required for the integration of the CAPTCHA API in your
-- JavaScript client applications. The API lets you customize the placement
-- and characteristics of the CAPTCHA puzzle for your end users. For more
-- information about the CAPTCHA JavaScript integration, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
--
-- /See:/ 'newAPIKeySummary' smart constructor.
data APIKeySummary = APIKeySummary'
  { -- | The generated, encrypted API key. You can copy this for use in your
    -- JavaScript CAPTCHA integration.
    aPIKey :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the key was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The token domains that are defined in this API key.
    tokenDomains :: Prelude.Maybe [Prelude.Text],
    -- | Internal value used by WAF to manage the key.
    version :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APIKeySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIKey', 'aPIKeySummary_aPIKey' - The generated, encrypted API key. You can copy this for use in your
-- JavaScript CAPTCHA integration.
--
-- 'creationTimestamp', 'aPIKeySummary_creationTimestamp' - The date and time that the key was created.
--
-- 'tokenDomains', 'aPIKeySummary_tokenDomains' - The token domains that are defined in this API key.
--
-- 'version', 'aPIKeySummary_version' - Internal value used by WAF to manage the key.
newAPIKeySummary ::
  APIKeySummary
newAPIKeySummary =
  APIKeySummary'
    { aPIKey = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      tokenDomains = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The generated, encrypted API key. You can copy this for use in your
-- JavaScript CAPTCHA integration.
aPIKeySummary_aPIKey :: Lens.Lens' APIKeySummary (Prelude.Maybe Prelude.Text)
aPIKeySummary_aPIKey = Lens.lens (\APIKeySummary' {aPIKey} -> aPIKey) (\s@APIKeySummary' {} a -> s {aPIKey = a} :: APIKeySummary)

-- | The date and time that the key was created.
aPIKeySummary_creationTimestamp :: Lens.Lens' APIKeySummary (Prelude.Maybe Prelude.UTCTime)
aPIKeySummary_creationTimestamp = Lens.lens (\APIKeySummary' {creationTimestamp} -> creationTimestamp) (\s@APIKeySummary' {} a -> s {creationTimestamp = a} :: APIKeySummary) Prelude.. Lens.mapping Data._Time

-- | The token domains that are defined in this API key.
aPIKeySummary_tokenDomains :: Lens.Lens' APIKeySummary (Prelude.Maybe [Prelude.Text])
aPIKeySummary_tokenDomains = Lens.lens (\APIKeySummary' {tokenDomains} -> tokenDomains) (\s@APIKeySummary' {} a -> s {tokenDomains = a} :: APIKeySummary) Prelude.. Lens.mapping Lens.coerced

-- | Internal value used by WAF to manage the key.
aPIKeySummary_version :: Lens.Lens' APIKeySummary (Prelude.Maybe Prelude.Natural)
aPIKeySummary_version = Lens.lens (\APIKeySummary' {version} -> version) (\s@APIKeySummary' {} a -> s {version = a} :: APIKeySummary)

instance Data.FromJSON APIKeySummary where
  parseJSON =
    Data.withObject
      "APIKeySummary"
      ( \x ->
          APIKeySummary'
            Prelude.<$> (x Data..:? "APIKey")
            Prelude.<*> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "TokenDomains" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable APIKeySummary where
  hashWithSalt _salt APIKeySummary' {..} =
    _salt
      `Prelude.hashWithSalt` aPIKey
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` tokenDomains
      `Prelude.hashWithSalt` version

instance Prelude.NFData APIKeySummary where
  rnf APIKeySummary' {..} =
    Prelude.rnf aPIKey
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf tokenDomains
      `Prelude.seq` Prelude.rnf version
