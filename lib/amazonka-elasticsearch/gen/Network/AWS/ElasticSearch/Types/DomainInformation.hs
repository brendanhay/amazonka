{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainInformation
  ( DomainInformation (..),

    -- * Smart constructor
    mkDomainInformation,

    -- * Lenses
    dOwnerId,
    dDomainName,
    dRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkDomainInformation' smart constructor.
data DomainInformation = DomainInformation'
  { ownerId :: Lude.Maybe Lude.Text,
    domainName :: Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainInformation' with the minimum fields required to make a request.
--
-- * 'ownerId' -
-- * 'domainName' -
-- * 'region' -
mkDomainInformation ::
  -- | 'domainName'
  Lude.Text ->
  DomainInformation
mkDomainInformation pDomainName_ =
  DomainInformation'
    { ownerId = Lude.Nothing,
      domainName = pDomainName_,
      region = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerId :: Lens.Lens' DomainInformation (Lude.Maybe Lude.Text)
dOwnerId = Lens.lens (ownerId :: DomainInformation -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: DomainInformation)
{-# DEPRECATED dOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DomainInformation Lude.Text
dDomainName = Lens.lens (domainName :: DomainInformation -> Lude.Text) (\s a -> s {domainName = a} :: DomainInformation)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRegion :: Lens.Lens' DomainInformation (Lude.Maybe Lude.Text)
dRegion = Lens.lens (region :: DomainInformation -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: DomainInformation)
{-# DEPRECATED dRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON DomainInformation where
  parseJSON =
    Lude.withObject
      "DomainInformation"
      ( \x ->
          DomainInformation'
            Lude.<$> (x Lude..:? "OwnerId")
            Lude.<*> (x Lude..: "DomainName")
            Lude.<*> (x Lude..:? "Region")
      )

instance Lude.ToJSON DomainInformation where
  toJSON DomainInformation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OwnerId" Lude..=) Lude.<$> ownerId,
            Lude.Just ("DomainName" Lude..= domainName),
            ("Region" Lude..=) Lude.<$> region
          ]
      )
