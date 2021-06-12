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
-- Module      : Network.AWS.ElasticSearch.Types.DomainInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainInformation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newDomainInformation' smart constructor.
data DomainInformation = DomainInformation'
  { ownerId :: Core.Maybe Core.Text,
    region :: Core.Maybe Core.Text,
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DomainInformation
newDomainInformation pDomainName_ =
  DomainInformation'
    { ownerId = Core.Nothing,
      region = Core.Nothing,
      domainName = pDomainName_
    }

-- | Undocumented member.
domainInformation_ownerId :: Lens.Lens' DomainInformation (Core.Maybe Core.Text)
domainInformation_ownerId = Lens.lens (\DomainInformation' {ownerId} -> ownerId) (\s@DomainInformation' {} a -> s {ownerId = a} :: DomainInformation)

-- | Undocumented member.
domainInformation_region :: Lens.Lens' DomainInformation (Core.Maybe Core.Text)
domainInformation_region = Lens.lens (\DomainInformation' {region} -> region) (\s@DomainInformation' {} a -> s {region = a} :: DomainInformation)

-- | Undocumented member.
domainInformation_domainName :: Lens.Lens' DomainInformation Core.Text
domainInformation_domainName = Lens.lens (\DomainInformation' {domainName} -> domainName) (\s@DomainInformation' {} a -> s {domainName = a} :: DomainInformation)

instance Core.FromJSON DomainInformation where
  parseJSON =
    Core.withObject
      "DomainInformation"
      ( \x ->
          DomainInformation'
            Core.<$> (x Core..:? "OwnerId")
            Core.<*> (x Core..:? "Region")
            Core.<*> (x Core..: "DomainName")
      )

instance Core.Hashable DomainInformation

instance Core.NFData DomainInformation

instance Core.ToJSON DomainInformation where
  toJSON DomainInformation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OwnerId" Core..=) Core.<$> ownerId,
            ("Region" Core..=) Core.<$> region,
            Core.Just ("DomainName" Core..= domainName)
          ]
      )
