{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainInfo
  ( DomainInfo (..),

    -- * Smart constructor
    mkDomainInfo,

    -- * Lenses
    diDomainName,
  )
where

import qualified Network.AWS.ElasticSearch.Types.DomainName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkDomainInfo' smart constructor.
newtype DomainInfo = DomainInfo'
  { -- | Specifies the @DomainName@ .
    domainName :: Core.Maybe Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DomainInfo' value with any optional fields omitted.
mkDomainInfo ::
  DomainInfo
mkDomainInfo = DomainInfo' {domainName = Core.Nothing}

-- | Specifies the @DomainName@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDomainName :: Lens.Lens' DomainInfo (Core.Maybe Types.DomainName)
diDomainName = Lens.field @"domainName"
{-# DEPRECATED diDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON DomainInfo where
  parseJSON =
    Core.withObject "DomainInfo" Core.$
      \x -> DomainInfo' Core.<$> (x Core..:? "DomainName")
