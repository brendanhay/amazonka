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
    dDomainName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkDomainInfo' smart constructor.
newtype DomainInfo = DomainInfo'
  { domainName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- * 'domainName' - Specifies the @DomainName@ .
mkDomainInfo ::
  DomainInfo
mkDomainInfo = DomainInfo' {domainName = Lude.Nothing}

-- | Specifies the @DomainName@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DomainInfo (Lude.Maybe Lude.Text)
dDomainName = Lens.lens (domainName :: DomainInfo -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DomainInfo)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromJSON DomainInfo where
  parseJSON =
    Lude.withObject
      "DomainInfo"
      (\x -> DomainInfo' Lude.<$> (x Lude..:? "DomainName"))
