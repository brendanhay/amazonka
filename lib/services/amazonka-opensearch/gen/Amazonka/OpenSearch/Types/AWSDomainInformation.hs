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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AWSDomainInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon OpenSearch Service domain.
--
-- /See:/ 'newAWSDomainInformation' smart constructor.
data AWSDomainInformation = AWSDomainInformation'
  { -- | The Amazon Web Services account ID of the domain owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region in which the domain is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | Name of the domain.
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
-- 'ownerId', 'aWSDomainInformation_ownerId' - The Amazon Web Services account ID of the domain owner.
--
-- 'region', 'aWSDomainInformation_region' - The Amazon Web Services Region in which the domain is located.
--
-- 'domainName', 'aWSDomainInformation_domainName' - Name of the domain.
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

-- | The Amazon Web Services account ID of the domain owner.
aWSDomainInformation_ownerId :: Lens.Lens' AWSDomainInformation (Prelude.Maybe Prelude.Text)
aWSDomainInformation_ownerId = Lens.lens (\AWSDomainInformation' {ownerId} -> ownerId) (\s@AWSDomainInformation' {} a -> s {ownerId = a} :: AWSDomainInformation)

-- | The Amazon Web Services Region in which the domain is located.
aWSDomainInformation_region :: Lens.Lens' AWSDomainInformation (Prelude.Maybe Prelude.Text)
aWSDomainInformation_region = Lens.lens (\AWSDomainInformation' {region} -> region) (\s@AWSDomainInformation' {} a -> s {region = a} :: AWSDomainInformation)

-- | Name of the domain.
aWSDomainInformation_domainName :: Lens.Lens' AWSDomainInformation Prelude.Text
aWSDomainInformation_domainName = Lens.lens (\AWSDomainInformation' {domainName} -> domainName) (\s@AWSDomainInformation' {} a -> s {domainName = a} :: AWSDomainInformation)

instance Data.FromJSON AWSDomainInformation where
  parseJSON =
    Data.withObject
      "AWSDomainInformation"
      ( \x ->
          AWSDomainInformation'
            Prelude.<$> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..: "DomainName")
      )

instance Prelude.Hashable AWSDomainInformation where
  hashWithSalt _salt AWSDomainInformation' {..} =
    _salt
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData AWSDomainInformation where
  rnf AWSDomainInformation' {..} =
    Prelude.rnf ownerId `Prelude.seq`
      Prelude.rnf region `Prelude.seq`
        Prelude.rnf domainName

instance Data.ToJSON AWSDomainInformation where
  toJSON AWSDomainInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("Region" Data..=) Prelude.<$> region,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )
