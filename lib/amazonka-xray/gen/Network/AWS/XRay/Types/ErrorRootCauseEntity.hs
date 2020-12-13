{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseEntity
  ( ErrorRootCauseEntity (..),

    -- * Smart constructor
    mkErrorRootCauseEntity,

    -- * Lenses
    erceExceptions,
    erceRemote,
    erceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a trace summary error.
--
-- /See:/ 'mkErrorRootCauseEntity' smart constructor.
data ErrorRootCauseEntity = ErrorRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Lude.Maybe [RootCauseException],
    -- | A flag that denotes a remote subsegment.
    remote :: Lude.Maybe Lude.Bool,
    -- | The name of the entity.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorRootCauseEntity' with the minimum fields required to make a request.
--
-- * 'exceptions' - The types and messages of the exceptions.
-- * 'remote' - A flag that denotes a remote subsegment.
-- * 'name' - The name of the entity.
mkErrorRootCauseEntity ::
  ErrorRootCauseEntity
mkErrorRootCauseEntity =
  ErrorRootCauseEntity'
    { exceptions = Lude.Nothing,
      remote = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The types and messages of the exceptions.
--
-- /Note:/ Consider using 'exceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erceExceptions :: Lens.Lens' ErrorRootCauseEntity (Lude.Maybe [RootCauseException])
erceExceptions = Lens.lens (exceptions :: ErrorRootCauseEntity -> Lude.Maybe [RootCauseException]) (\s a -> s {exceptions = a} :: ErrorRootCauseEntity)
{-# DEPRECATED erceExceptions "Use generic-lens or generic-optics with 'exceptions' instead." #-}

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erceRemote :: Lens.Lens' ErrorRootCauseEntity (Lude.Maybe Lude.Bool)
erceRemote = Lens.lens (remote :: ErrorRootCauseEntity -> Lude.Maybe Lude.Bool) (\s a -> s {remote = a} :: ErrorRootCauseEntity)
{-# DEPRECATED erceRemote "Use generic-lens or generic-optics with 'remote' instead." #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erceName :: Lens.Lens' ErrorRootCauseEntity (Lude.Maybe Lude.Text)
erceName = Lens.lens (name :: ErrorRootCauseEntity -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ErrorRootCauseEntity)
{-# DEPRECATED erceName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ErrorRootCauseEntity where
  parseJSON =
    Lude.withObject
      "ErrorRootCauseEntity"
      ( \x ->
          ErrorRootCauseEntity'
            Lude.<$> (x Lude..:? "Exceptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Remote")
            Lude.<*> (x Lude..:? "Name")
      )
