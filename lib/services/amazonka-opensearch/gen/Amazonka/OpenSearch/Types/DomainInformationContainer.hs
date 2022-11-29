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
-- Module      : Amazonka.OpenSearch.Types.DomainInformationContainer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainInformationContainer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.AWSDomainInformation
import qualified Amazonka.Prelude as Prelude

-- | Container for information about an OpenSearch Service domain.
--
-- /See:/ 'newDomainInformationContainer' smart constructor.
data DomainInformationContainer = DomainInformationContainer'
  { -- | Information about an Amazon OpenSearch Service domain.
    aWSDomainInformation :: Prelude.Maybe AWSDomainInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainInformationContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aWSDomainInformation', 'domainInformationContainer_aWSDomainInformation' - Information about an Amazon OpenSearch Service domain.
newDomainInformationContainer ::
  DomainInformationContainer
newDomainInformationContainer =
  DomainInformationContainer'
    { aWSDomainInformation =
        Prelude.Nothing
    }

-- | Information about an Amazon OpenSearch Service domain.
domainInformationContainer_aWSDomainInformation :: Lens.Lens' DomainInformationContainer (Prelude.Maybe AWSDomainInformation)
domainInformationContainer_aWSDomainInformation = Lens.lens (\DomainInformationContainer' {aWSDomainInformation} -> aWSDomainInformation) (\s@DomainInformationContainer' {} a -> s {aWSDomainInformation = a} :: DomainInformationContainer)

instance Core.FromJSON DomainInformationContainer where
  parseJSON =
    Core.withObject
      "DomainInformationContainer"
      ( \x ->
          DomainInformationContainer'
            Prelude.<$> (x Core..:? "AWSDomainInformation")
      )

instance Prelude.Hashable DomainInformationContainer where
  hashWithSalt _salt DomainInformationContainer' {..} =
    _salt `Prelude.hashWithSalt` aWSDomainInformation

instance Prelude.NFData DomainInformationContainer where
  rnf DomainInformationContainer' {..} =
    Prelude.rnf aWSDomainInformation

instance Core.ToJSON DomainInformationContainer where
  toJSON DomainInformationContainer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AWSDomainInformation" Core..=)
              Prelude.<$> aWSDomainInformation
          ]
      )
