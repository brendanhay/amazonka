{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionsList
  ( ConnectionsList (..),

    -- * Smart constructor
    mkConnectionsList,

    -- * Lenses
    clConnections,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the connections used by a job.
--
-- /See:/ 'mkConnectionsList' smart constructor.
newtype ConnectionsList = ConnectionsList'
  { connections ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionsList' with the minimum fields required to make a request.
--
-- * 'connections' - A list of connections used by the job.
mkConnectionsList ::
  ConnectionsList
mkConnectionsList = ConnectionsList' {connections = Lude.Nothing}

-- | A list of connections used by the job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConnections :: Lens.Lens' ConnectionsList (Lude.Maybe [Lude.Text])
clConnections = Lens.lens (connections :: ConnectionsList -> Lude.Maybe [Lude.Text]) (\s a -> s {connections = a} :: ConnectionsList)
{-# DEPRECATED clConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

instance Lude.FromJSON ConnectionsList where
  parseJSON =
    Lude.withObject
      "ConnectionsList"
      ( \x ->
          ConnectionsList'
            Lude.<$> (x Lude..:? "Connections" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ConnectionsList where
  toJSON ConnectionsList' {..} =
    Lude.object
      (Lude.catMaybes [("Connections" Lude..=) Lude.<$> connections])
