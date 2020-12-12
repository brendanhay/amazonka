{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EngineVersion
  ( EngineVersion (..),

    -- * Smart constructor
    mkEngineVersion,

    -- * Lenses
    evName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Id of the engine version.
--
-- /See:/ 'mkEngineVersion' smart constructor.
newtype EngineVersion = EngineVersion'
  { name ::
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

-- | Creates a value of 'EngineVersion' with the minimum fields required to make a request.
--
-- * 'name' - Id for the version.
mkEngineVersion ::
  EngineVersion
mkEngineVersion = EngineVersion' {name = Lude.Nothing}

-- | Id for the version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evName :: Lens.Lens' EngineVersion (Lude.Maybe Lude.Text)
evName = Lens.lens (name :: EngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: EngineVersion)
{-# DEPRECATED evName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON EngineVersion where
  parseJSON =
    Lude.withObject
      "EngineVersion"
      (\x -> EngineVersion' Lude.<$> (x Lude..:? "name"))
