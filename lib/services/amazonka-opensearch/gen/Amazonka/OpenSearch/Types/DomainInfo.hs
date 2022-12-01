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
-- Module      : Amazonka.OpenSearch.Types.DomainInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.EngineType
import qualified Amazonka.Prelude as Prelude

-- | Information about an OpenSearch Service domain.
--
-- /See:/ 'newDomainInfo' smart constructor.
data DomainInfo = DomainInfo'
  { -- | The type of search engine that the domain is running.@OpenSearch@ for an
    -- OpenSearch engine, or @Elasticsearch@ for a legacy Elasticsearch OSS
    -- engine.
    engineType :: Prelude.Maybe EngineType,
    -- | Name of the domain.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'domainInfo_engineType' - The type of search engine that the domain is running.@OpenSearch@ for an
-- OpenSearch engine, or @Elasticsearch@ for a legacy Elasticsearch OSS
-- engine.
--
-- 'domainName', 'domainInfo_domainName' - Name of the domain.
newDomainInfo ::
  DomainInfo
newDomainInfo =
  DomainInfo'
    { engineType = Prelude.Nothing,
      domainName = Prelude.Nothing
    }

-- | The type of search engine that the domain is running.@OpenSearch@ for an
-- OpenSearch engine, or @Elasticsearch@ for a legacy Elasticsearch OSS
-- engine.
domainInfo_engineType :: Lens.Lens' DomainInfo (Prelude.Maybe EngineType)
domainInfo_engineType = Lens.lens (\DomainInfo' {engineType} -> engineType) (\s@DomainInfo' {} a -> s {engineType = a} :: DomainInfo)

-- | Name of the domain.
domainInfo_domainName :: Lens.Lens' DomainInfo (Prelude.Maybe Prelude.Text)
domainInfo_domainName = Lens.lens (\DomainInfo' {domainName} -> domainName) (\s@DomainInfo' {} a -> s {domainName = a} :: DomainInfo)

instance Core.FromJSON DomainInfo where
  parseJSON =
    Core.withObject
      "DomainInfo"
      ( \x ->
          DomainInfo'
            Prelude.<$> (x Core..:? "EngineType")
            Prelude.<*> (x Core..:? "DomainName")
      )

instance Prelude.Hashable DomainInfo where
  hashWithSalt _salt DomainInfo' {..} =
    _salt `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DomainInfo where
  rnf DomainInfo' {..} =
    Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf domainName
