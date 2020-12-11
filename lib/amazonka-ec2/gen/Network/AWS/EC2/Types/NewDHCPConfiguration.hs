-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NewDHCPConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NewDHCPConfiguration
  ( NewDHCPConfiguration (..),

    -- * Smart constructor
    mkNewDHCPConfiguration,

    -- * Lenses
    ndcValues,
    ndcKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkNewDHCPConfiguration' smart constructor.
data NewDHCPConfiguration = NewDHCPConfiguration'
  { values ::
      Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewDHCPConfiguration' with the minimum fields required to make a request.
--
-- * 'key' - Undocumented field.
-- * 'values' - Undocumented field.
mkNewDHCPConfiguration ::
  NewDHCPConfiguration
mkNewDHCPConfiguration =
  NewDHCPConfiguration' {values = Lude.Nothing, key = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndcValues :: Lens.Lens' NewDHCPConfiguration (Lude.Maybe [Lude.Text])
ndcValues = Lens.lens (values :: NewDHCPConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: NewDHCPConfiguration)
{-# DEPRECATED ndcValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndcKey :: Lens.Lens' NewDHCPConfiguration (Lude.Maybe Lude.Text)
ndcKey = Lens.lens (key :: NewDHCPConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: NewDHCPConfiguration)
{-# DEPRECATED ndcKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToQuery NewDHCPConfiguration where
  toQuery NewDHCPConfiguration' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Value" Lude.<$> values),
        "Key" Lude.=: key
      ]
