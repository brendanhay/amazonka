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
-- Module      : Amazonka.OpenSearch.Types.AWSDomainInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AWSDomainInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAWSDomainInformation' smart constructor.
data AWSDomainInformation = AWSDomainInformation'
  { ownerId :: Prelude.Maybe Prelude.Text,
    region :: Prelude.Maybe Prelude.Text,
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSDomainInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'aWSDomainInformation_ownerId' - Undocumented member.
--
-- 'region', 'aWSDomainInformation_region' - Undocumented member.
--
-- 'domainName', 'aWSDomainInformation_domainName' - Undocumented member.
newAWSDomainInformation ::
  -- | 'domainName'
  Prelude.Text ->
  AWSDomainInformation
newAWSDomainInformation pDomainName_ =
  AWSDomainInformation'
    { ownerId = Prelude.Nothing,
      region = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Undocumented member.
aWSDomainInformation_ownerId :: Lens.Lens' AWSDomainInformation (Prelude.Maybe Prelude.Text)
aWSDomainInformation_ownerId = Lens.lens (\AWSDomainInformation' {ownerId} -> ownerId) (\s@AWSDomainInformation' {} a -> s {ownerId = a} :: AWSDomainInformation)

-- | Undocumented member.
aWSDomainInformation_region :: Lens.Lens' AWSDomainInformation (Prelude.Maybe Prelude.Text)
aWSDomainInformation_region = Lens.lens (\AWSDomainInformation' {region} -> region) (\s@AWSDomainInformation' {} a -> s {region = a} :: AWSDomainInformation)

-- | Undocumented member.
aWSDomainInformation_domainName :: Lens.Lens' AWSDomainInformation Prelude.Text
aWSDomainInformation_domainName = Lens.lens (\AWSDomainInformation' {domainName} -> domainName) (\s@AWSDomainInformation' {} a -> s {domainName = a} :: AWSDomainInformation)

instance Core.FromJSON AWSDomainInformation where
  parseJSON =
    Core.withObject
      "AWSDomainInformation"
      ( \x ->
          AWSDomainInformation'
            Prelude.<$> (x Core..:? "OwnerId")
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..: "DomainName")
      )

instance Prelude.Hashable AWSDomainInformation where
  hashWithSalt _salt AWSDomainInformation' {..} =
    _salt `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData AWSDomainInformation where
  rnf AWSDomainInformation' {..} =
    Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToJSON AWSDomainInformation where
  toJSON AWSDomainInformation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OwnerId" Core..=) Prelude.<$> ownerId,
            ("Region" Core..=) Prelude.<$> region,
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )
