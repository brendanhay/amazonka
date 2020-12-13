{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Evidence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Evidence
  ( Evidence (..),

    -- * Smart constructor
    mkEvidence,

    -- * Lenses
    eThreatIntelligenceDetails,
  )
where

import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the reason that the finding was generated.
--
-- /See:/ 'mkEvidence' smart constructor.
newtype Evidence = Evidence'
  { -- | A list of threat intelligence details related to the evidence.
    threatIntelligenceDetails :: Lude.Maybe [ThreatIntelligenceDetail]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Evidence' with the minimum fields required to make a request.
--
-- * 'threatIntelligenceDetails' - A list of threat intelligence details related to the evidence.
mkEvidence ::
  Evidence
mkEvidence = Evidence' {threatIntelligenceDetails = Lude.Nothing}

-- | A list of threat intelligence details related to the evidence.
--
-- /Note:/ Consider using 'threatIntelligenceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eThreatIntelligenceDetails :: Lens.Lens' Evidence (Lude.Maybe [ThreatIntelligenceDetail])
eThreatIntelligenceDetails = Lens.lens (threatIntelligenceDetails :: Evidence -> Lude.Maybe [ThreatIntelligenceDetail]) (\s a -> s {threatIntelligenceDetails = a} :: Evidence)
{-# DEPRECATED eThreatIntelligenceDetails "Use generic-lens or generic-optics with 'threatIntelligenceDetails' instead." #-}

instance Lude.FromJSON Evidence where
  parseJSON =
    Lude.withObject
      "Evidence"
      ( \x ->
          Evidence'
            Lude.<$> (x Lude..:? "threatIntelligenceDetails" Lude..!= Lude.mempty)
      )
