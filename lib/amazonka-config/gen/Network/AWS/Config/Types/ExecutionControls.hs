-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ExecutionControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ExecutionControls
  ( ExecutionControls (..),

    -- * Smart constructor
    mkExecutionControls,

    -- * Lenses
    ecSsmControls,
  )
where

import Network.AWS.Config.Types.SsmControls
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The controls that AWS Config uses for executing remediations.
--
-- /See:/ 'mkExecutionControls' smart constructor.
newtype ExecutionControls = ExecutionControls'
  { ssmControls ::
      Lude.Maybe SsmControls
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionControls' with the minimum fields required to make a request.
--
-- * 'ssmControls' - A SsmControls object.
mkExecutionControls ::
  ExecutionControls
mkExecutionControls =
  ExecutionControls' {ssmControls = Lude.Nothing}

-- | A SsmControls object.
--
-- /Note:/ Consider using 'ssmControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecSsmControls :: Lens.Lens' ExecutionControls (Lude.Maybe SsmControls)
ecSsmControls = Lens.lens (ssmControls :: ExecutionControls -> Lude.Maybe SsmControls) (\s a -> s {ssmControls = a} :: ExecutionControls)
{-# DEPRECATED ecSsmControls "Use generic-lens or generic-optics with 'ssmControls' instead." #-}

instance Lude.FromJSON ExecutionControls where
  parseJSON =
    Lude.withObject
      "ExecutionControls"
      (\x -> ExecutionControls' Lude.<$> (x Lude..:? "SsmControls"))

instance Lude.ToJSON ExecutionControls where
  toJSON ExecutionControls' {..} =
    Lude.object
      (Lude.catMaybes [("SsmControls" Lude..=) Lude.<$> ssmControls])
