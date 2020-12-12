{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.CoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.CoreDefinitionVersion
  ( CoreDefinitionVersion (..),

    -- * Smart constructor
    mkCoreDefinitionVersion,

    -- * Lenses
    cdvCores,
  )
where

import Network.AWS.Greengrass.Types.Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a core definition version.
--
-- /See:/ 'mkCoreDefinitionVersion' smart constructor.
newtype CoreDefinitionVersion = CoreDefinitionVersion'
  { cores ::
      Lude.Maybe [Core]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoreDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'cores' - A list of cores in the core definition version.
mkCoreDefinitionVersion ::
  CoreDefinitionVersion
mkCoreDefinitionVersion =
  CoreDefinitionVersion' {cores = Lude.Nothing}

-- | A list of cores in the core definition version.
--
-- /Note:/ Consider using 'cores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvCores :: Lens.Lens' CoreDefinitionVersion (Lude.Maybe [Core])
cdvCores = Lens.lens (cores :: CoreDefinitionVersion -> Lude.Maybe [Core]) (\s a -> s {cores = a} :: CoreDefinitionVersion)
{-# DEPRECATED cdvCores "Use generic-lens or generic-optics with 'cores' instead." #-}

instance Lude.FromJSON CoreDefinitionVersion where
  parseJSON =
    Lude.withObject
      "CoreDefinitionVersion"
      ( \x ->
          CoreDefinitionVersion'
            Lude.<$> (x Lude..:? "Cores" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CoreDefinitionVersion where
  toJSON CoreDefinitionVersion' {..} =
    Lude.object (Lude.catMaybes [("Cores" Lude..=) Lude.<$> cores])
