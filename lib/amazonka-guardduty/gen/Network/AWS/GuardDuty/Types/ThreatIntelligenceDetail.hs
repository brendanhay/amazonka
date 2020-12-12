{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
  ( ThreatIntelligenceDetail (..),

    -- * Smart constructor
    mkThreatIntelligenceDetail,

    -- * Lenses
    tidThreatNames,
    tidThreatListName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An instance of a threat intelligence detail that constitutes evidence for the finding.
--
-- /See:/ 'mkThreatIntelligenceDetail' smart constructor.
data ThreatIntelligenceDetail = ThreatIntelligenceDetail'
  { threatNames ::
      Lude.Maybe [Lude.Text],
    threatListName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThreatIntelligenceDetail' with the minimum fields required to make a request.
--
-- * 'threatListName' - The name of the threat intelligence list that triggered the finding.
-- * 'threatNames' - A list of names of the threats in the threat intelligence list that triggered the finding.
mkThreatIntelligenceDetail ::
  ThreatIntelligenceDetail
mkThreatIntelligenceDetail =
  ThreatIntelligenceDetail'
    { threatNames = Lude.Nothing,
      threatListName = Lude.Nothing
    }

-- | A list of names of the threats in the threat intelligence list that triggered the finding.
--
-- /Note:/ Consider using 'threatNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidThreatNames :: Lens.Lens' ThreatIntelligenceDetail (Lude.Maybe [Lude.Text])
tidThreatNames = Lens.lens (threatNames :: ThreatIntelligenceDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {threatNames = a} :: ThreatIntelligenceDetail)
{-# DEPRECATED tidThreatNames "Use generic-lens or generic-optics with 'threatNames' instead." #-}

-- | The name of the threat intelligence list that triggered the finding.
--
-- /Note:/ Consider using 'threatListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidThreatListName :: Lens.Lens' ThreatIntelligenceDetail (Lude.Maybe Lude.Text)
tidThreatListName = Lens.lens (threatListName :: ThreatIntelligenceDetail -> Lude.Maybe Lude.Text) (\s a -> s {threatListName = a} :: ThreatIntelligenceDetail)
{-# DEPRECATED tidThreatListName "Use generic-lens or generic-optics with 'threatListName' instead." #-}

instance Lude.FromJSON ThreatIntelligenceDetail where
  parseJSON =
    Lude.withObject
      "ThreatIntelligenceDetail"
      ( \x ->
          ThreatIntelligenceDetail'
            Lude.<$> (x Lude..:? "threatNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "threatListName")
      )
