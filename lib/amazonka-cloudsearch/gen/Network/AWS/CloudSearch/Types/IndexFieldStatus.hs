{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexFieldStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexFieldStatus
  ( IndexFieldStatus (..),

    -- * Smart constructor
    mkIndexFieldStatus,

    -- * Lenses
    ifsOptions,
    ifsStatus,
  )
where

import Network.AWS.CloudSearch.Types.IndexField
import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The value of an @IndexField@ and its current status.
--
-- /See:/ 'mkIndexFieldStatus' smart constructor.
data IndexFieldStatus = IndexFieldStatus'
  { options :: IndexField,
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

-- | Creates a value of 'IndexFieldStatus' with the minimum fields required to make a request.
--
-- * 'options' - Undocumented field.
-- * 'status' - Undocumented field.
mkIndexFieldStatus ::
  -- | 'options'
  IndexField ->
  -- | 'status'
  OptionStatus ->
  IndexFieldStatus
mkIndexFieldStatus pOptions_ pStatus_ =
  IndexFieldStatus' {options = pOptions_, status = pStatus_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsOptions :: Lens.Lens' IndexFieldStatus IndexField
ifsOptions = Lens.lens (options :: IndexFieldStatus -> IndexField) (\s a -> s {options = a} :: IndexFieldStatus)
{-# DEPRECATED ifsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsStatus :: Lens.Lens' IndexFieldStatus OptionStatus
ifsStatus = Lens.lens (status :: IndexFieldStatus -> OptionStatus) (\s a -> s {status = a} :: IndexFieldStatus)
{-# DEPRECATED ifsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML IndexFieldStatus where
  parseXML x =
    IndexFieldStatus'
      Lude.<$> (x Lude..@ "Options") Lude.<*> (x Lude..@ "Status")
