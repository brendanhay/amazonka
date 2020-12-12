{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseEntity
  ( FaultRootCauseEntity (..),

    -- * Smart constructor
    mkFaultRootCauseEntity,

    -- * Lenses
    frceExceptions,
    frceRemote,
    frceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a trace summary fault error.
--
-- /See:/ 'mkFaultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { exceptions ::
      Lude.Maybe [RootCauseException],
    remote :: Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaultRootCauseEntity' with the minimum fields required to make a request.
--
-- * 'exceptions' - The types and messages of the exceptions.
-- * 'name' - The name of the entity.
-- * 'remote' - A flag that denotes a remote subsegment.
mkFaultRootCauseEntity ::
  FaultRootCauseEntity
mkFaultRootCauseEntity =
  FaultRootCauseEntity'
    { exceptions = Lude.Nothing,
      remote = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The types and messages of the exceptions.
--
-- /Note:/ Consider using 'exceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceExceptions :: Lens.Lens' FaultRootCauseEntity (Lude.Maybe [RootCauseException])
frceExceptions = Lens.lens (exceptions :: FaultRootCauseEntity -> Lude.Maybe [RootCauseException]) (\s a -> s {exceptions = a} :: FaultRootCauseEntity)
{-# DEPRECATED frceExceptions "Use generic-lens or generic-optics with 'exceptions' instead." #-}

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceRemote :: Lens.Lens' FaultRootCauseEntity (Lude.Maybe Lude.Bool)
frceRemote = Lens.lens (remote :: FaultRootCauseEntity -> Lude.Maybe Lude.Bool) (\s a -> s {remote = a} :: FaultRootCauseEntity)
{-# DEPRECATED frceRemote "Use generic-lens or generic-optics with 'remote' instead." #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceName :: Lens.Lens' FaultRootCauseEntity (Lude.Maybe Lude.Text)
frceName = Lens.lens (name :: FaultRootCauseEntity -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FaultRootCauseEntity)
{-# DEPRECATED frceName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON FaultRootCauseEntity where
  parseJSON =
    Lude.withObject
      "FaultRootCauseEntity"
      ( \x ->
          FaultRootCauseEntity'
            Lude.<$> (x Lude..:? "Exceptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Remote")
            Lude.<*> (x Lude..:? "Name")
      )
