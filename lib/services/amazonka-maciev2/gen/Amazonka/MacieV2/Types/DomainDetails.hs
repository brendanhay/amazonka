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
-- Module      : Amazonka.MacieV2.Types.DomainDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.DomainDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the domain name of the device that an entity
-- used to perform an action on an affected resource.
--
-- /See:/ 'newDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The name of the domain.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'domainDetails_domainName' - The name of the domain.
newDomainDetails ::
  DomainDetails
newDomainDetails =
  DomainDetails' {domainName = Prelude.Nothing}

-- | The name of the domain.
domainDetails_domainName :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainName = Lens.lens (\DomainDetails' {domainName} -> domainName) (\s@DomainDetails' {} a -> s {domainName = a} :: DomainDetails)

instance Data.FromJSON DomainDetails where
  parseJSON =
    Data.withObject
      "DomainDetails"
      ( \x ->
          DomainDetails' Prelude.<$> (x Data..:? "domainName")
      )

instance Prelude.Hashable DomainDetails where
  hashWithSalt _salt DomainDetails' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DomainDetails where
  rnf DomainDetails' {..} = Prelude.rnf domainName
