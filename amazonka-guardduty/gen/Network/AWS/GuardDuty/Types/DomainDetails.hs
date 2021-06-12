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
-- Module      : Network.AWS.GuardDuty.Types.DomainDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DomainDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the domain.
--
-- /See:/ 'newDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The domain information for the AWS API call.
    domain :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'domainDetails_domain' - The domain information for the AWS API call.
newDomainDetails ::
  DomainDetails
newDomainDetails =
  DomainDetails' {domain = Core.Nothing}

-- | The domain information for the AWS API call.
domainDetails_domain :: Lens.Lens' DomainDetails (Core.Maybe Core.Text)
domainDetails_domain = Lens.lens (\DomainDetails' {domain} -> domain) (\s@DomainDetails' {} a -> s {domain = a} :: DomainDetails)

instance Core.FromJSON DomainDetails where
  parseJSON =
    Core.withObject
      "DomainDetails"
      ( \x ->
          DomainDetails' Core.<$> (x Core..:? "domain")
      )

instance Core.Hashable DomainDetails

instance Core.NFData DomainDetails
