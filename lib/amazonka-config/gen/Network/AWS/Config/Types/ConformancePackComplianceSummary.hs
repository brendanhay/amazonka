-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceSummary
  ( ConformancePackComplianceSummary (..),

    -- * Smart constructor
    mkConformancePackComplianceSummary,

    -- * Lenses
    cpcsConformancePackName,
    cpcsConformancePackComplianceStatus,
  )
where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary includes the name and status of the conformance pack.
--
-- /See:/ 'mkConformancePackComplianceSummary' smart constructor.
data ConformancePackComplianceSummary = ConformancePackComplianceSummary'
  { conformancePackName ::
      Lude.Text,
    conformancePackComplianceStatus ::
      ConformancePackComplianceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackComplianceSummary' with the minimum fields required to make a request.
--
-- * 'conformancePackComplianceStatus' - The status of the conformance pack. The allowed values are COMPLIANT and NON_COMPLIANT.
-- * 'conformancePackName' - The name of the conformance pack name.
mkConformancePackComplianceSummary ::
  -- | 'conformancePackName'
  Lude.Text ->
  -- | 'conformancePackComplianceStatus'
  ConformancePackComplianceType ->
  ConformancePackComplianceSummary
mkConformancePackComplianceSummary
  pConformancePackName_
  pConformancePackComplianceStatus_ =
    ConformancePackComplianceSummary'
      { conformancePackName =
          pConformancePackName_,
        conformancePackComplianceStatus =
          pConformancePackComplianceStatus_
      }

-- | The name of the conformance pack name.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcsConformancePackName :: Lens.Lens' ConformancePackComplianceSummary Lude.Text
cpcsConformancePackName = Lens.lens (conformancePackName :: ConformancePackComplianceSummary -> Lude.Text) (\s a -> s {conformancePackName = a} :: ConformancePackComplianceSummary)
{-# DEPRECATED cpcsConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | The status of the conformance pack. The allowed values are COMPLIANT and NON_COMPLIANT.
--
-- /Note:/ Consider using 'conformancePackComplianceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcsConformancePackComplianceStatus :: Lens.Lens' ConformancePackComplianceSummary ConformancePackComplianceType
cpcsConformancePackComplianceStatus = Lens.lens (conformancePackComplianceStatus :: ConformancePackComplianceSummary -> ConformancePackComplianceType) (\s a -> s {conformancePackComplianceStatus = a} :: ConformancePackComplianceSummary)
{-# DEPRECATED cpcsConformancePackComplianceStatus "Use generic-lens or generic-optics with 'conformancePackComplianceStatus' instead." #-}

instance Lude.FromJSON ConformancePackComplianceSummary where
  parseJSON =
    Lude.withObject
      "ConformancePackComplianceSummary"
      ( \x ->
          ConformancePackComplianceSummary'
            Lude.<$> (x Lude..: "ConformancePackName")
            Lude.<*> (x Lude..: "ConformancePackComplianceStatus")
      )
