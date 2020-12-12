{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
  ( AnalysisSchemeStatus (..),

    -- * Smart constructor
    mkAnalysisSchemeStatus,

    -- * Lenses
    assOptions,
    assStatus,
  )
where

import Network.AWS.CloudSearch.Types.AnalysisScheme
import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status and configuration of an @AnalysisScheme@ .
--
-- /See:/ 'mkAnalysisSchemeStatus' smart constructor.
data AnalysisSchemeStatus = AnalysisSchemeStatus'
  { options ::
      AnalysisScheme,
    status :: OptionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalysisSchemeStatus' with the minimum fields required to make a request.
--
-- * 'options' - Undocumented field.
-- * 'status' - Undocumented field.
mkAnalysisSchemeStatus ::
  -- | 'options'
  AnalysisScheme ->
  -- | 'status'
  OptionStatus ->
  AnalysisSchemeStatus
mkAnalysisSchemeStatus pOptions_ pStatus_ =
  AnalysisSchemeStatus' {options = pOptions_, status = pStatus_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assOptions :: Lens.Lens' AnalysisSchemeStatus AnalysisScheme
assOptions = Lens.lens (options :: AnalysisSchemeStatus -> AnalysisScheme) (\s a -> s {options = a} :: AnalysisSchemeStatus)
{-# DEPRECATED assOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assStatus :: Lens.Lens' AnalysisSchemeStatus OptionStatus
assStatus = Lens.lens (status :: AnalysisSchemeStatus -> OptionStatus) (\s a -> s {status = a} :: AnalysisSchemeStatus)
{-# DEPRECATED assStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML AnalysisSchemeStatus where
  parseXML x =
    AnalysisSchemeStatus'
      Lude.<$> (x Lude..@ "Options") Lude.<*> (x Lude..@ "Status")
