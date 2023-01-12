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
-- Module      : Amazonka.ElasticSearch.Types.DomainInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.DomainInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDomainInformation' smart constructor.
data DomainInformation = DomainInformation'
  { ownerId :: Prelude.Maybe Prelude.Text,
    region :: Prelude.Maybe Prelude.Text,
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'domainInformation_ownerId' - Undocumented member.
--
-- 'region', 'domainInformation_region' - Undocumented member.
--
-- 'domainName', 'domainInformation_domainName' - Undocumented member.
newDomainInformation ::
  -- | 'domainName'
  Prelude.Text ->
  DomainInformation
newDomainInformation pDomainName_ =
  DomainInformation'
    { ownerId = Prelude.Nothing,
      region = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Undocumented member.
domainInformation_ownerId :: Lens.Lens' DomainInformation (Prelude.Maybe Prelude.Text)
domainInformation_ownerId = Lens.lens (\DomainInformation' {ownerId} -> ownerId) (\s@DomainInformation' {} a -> s {ownerId = a} :: DomainInformation)

-- | Undocumented member.
domainInformation_region :: Lens.Lens' DomainInformation (Prelude.Maybe Prelude.Text)
domainInformation_region = Lens.lens (\DomainInformation' {region} -> region) (\s@DomainInformation' {} a -> s {region = a} :: DomainInformation)

-- | Undocumented member.
domainInformation_domainName :: Lens.Lens' DomainInformation Prelude.Text
domainInformation_domainName = Lens.lens (\DomainInformation' {domainName} -> domainName) (\s@DomainInformation' {} a -> s {domainName = a} :: DomainInformation)

instance Data.FromJSON DomainInformation where
  parseJSON =
    Data.withObject
      "DomainInformation"
      ( \x ->
          DomainInformation'
            Prelude.<$> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..: "DomainName")
      )

instance Prelude.Hashable DomainInformation where
  hashWithSalt _salt DomainInformation' {..} =
    _salt `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DomainInformation where
  rnf DomainInformation' {..} =
    Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToJSON DomainInformation where
  toJSON DomainInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("Region" Data..=) Prelude.<$> region,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )
