{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
  ( ResponseTimeRootCauseEntity (..),

    -- * Smart constructor
    mkResponseTimeRootCauseEntity,

    -- * Lenses
    rtrceRemote,
    rtrceCoverage,
    rtrceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of segments and corresponding subsegments associated to a response time warning.
--
-- /See:/ 'mkResponseTimeRootCauseEntity' smart constructor.
data ResponseTimeRootCauseEntity = ResponseTimeRootCauseEntity'
  { -- | A flag that denotes a remote subsegment.
    remote :: Lude.Maybe Lude.Bool,
    -- | The type and messages of the exceptions.
    coverage :: Lude.Maybe Lude.Double,
    -- | The name of the entity.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResponseTimeRootCauseEntity' with the minimum fields required to make a request.
--
-- * 'remote' - A flag that denotes a remote subsegment.
-- * 'coverage' - The type and messages of the exceptions.
-- * 'name' - The name of the entity.
mkResponseTimeRootCauseEntity ::
  ResponseTimeRootCauseEntity
mkResponseTimeRootCauseEntity =
  ResponseTimeRootCauseEntity'
    { remote = Lude.Nothing,
      coverage = Lude.Nothing,
      name = Lude.Nothing
    }

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrceRemote :: Lens.Lens' ResponseTimeRootCauseEntity (Lude.Maybe Lude.Bool)
rtrceRemote = Lens.lens (remote :: ResponseTimeRootCauseEntity -> Lude.Maybe Lude.Bool) (\s a -> s {remote = a} :: ResponseTimeRootCauseEntity)
{-# DEPRECATED rtrceRemote "Use generic-lens or generic-optics with 'remote' instead." #-}

-- | The type and messages of the exceptions.
--
-- /Note:/ Consider using 'coverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrceCoverage :: Lens.Lens' ResponseTimeRootCauseEntity (Lude.Maybe Lude.Double)
rtrceCoverage = Lens.lens (coverage :: ResponseTimeRootCauseEntity -> Lude.Maybe Lude.Double) (\s a -> s {coverage = a} :: ResponseTimeRootCauseEntity)
{-# DEPRECATED rtrceCoverage "Use generic-lens or generic-optics with 'coverage' instead." #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrceName :: Lens.Lens' ResponseTimeRootCauseEntity (Lude.Maybe Lude.Text)
rtrceName = Lens.lens (name :: ResponseTimeRootCauseEntity -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResponseTimeRootCauseEntity)
{-# DEPRECATED rtrceName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ResponseTimeRootCauseEntity where
  parseJSON =
    Lude.withObject
      "ResponseTimeRootCauseEntity"
      ( \x ->
          ResponseTimeRootCauseEntity'
            Lude.<$> (x Lude..:? "Remote")
            Lude.<*> (x Lude..:? "Coverage")
            Lude.<*> (x Lude..:? "Name")
      )
