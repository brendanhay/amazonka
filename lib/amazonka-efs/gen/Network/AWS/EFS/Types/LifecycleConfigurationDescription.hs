{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifecycleConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecycleConfigurationDescription
  ( LifecycleConfigurationDescription (..),

    -- * Smart constructor
    mkLifecycleConfigurationDescription,

    -- * Lenses
    lcdLifecyclePolicies,
  )
where

import Network.AWS.EFS.Types.LifecyclePolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkLifecycleConfigurationDescription' smart constructor.
newtype LifecycleConfigurationDescription = LifecycleConfigurationDescription'
  { lifecyclePolicies ::
      Lude.Maybe
        [LifecyclePolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleConfigurationDescription' with the minimum fields required to make a request.
--
-- * 'lifecyclePolicies' - An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
mkLifecycleConfigurationDescription ::
  LifecycleConfigurationDescription
mkLifecycleConfigurationDescription =
  LifecycleConfigurationDescription'
    { lifecyclePolicies =
        Lude.Nothing
    }

-- | An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
--
-- /Note:/ Consider using 'lifecyclePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdLifecyclePolicies :: Lens.Lens' LifecycleConfigurationDescription (Lude.Maybe [LifecyclePolicy])
lcdLifecyclePolicies = Lens.lens (lifecyclePolicies :: LifecycleConfigurationDescription -> Lude.Maybe [LifecyclePolicy]) (\s a -> s {lifecyclePolicies = a} :: LifecycleConfigurationDescription)
{-# DEPRECATED lcdLifecyclePolicies "Use generic-lens or generic-optics with 'lifecyclePolicies' instead." #-}

instance Lude.FromJSON LifecycleConfigurationDescription where
  parseJSON =
    Lude.withObject
      "LifecycleConfigurationDescription"
      ( \x ->
          LifecycleConfigurationDescription'
            Lude.<$> (x Lude..:? "LifecyclePolicies" Lude..!= Lude.mempty)
      )
