{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ComputeType
  ( ComputeType (..),

    -- * Smart constructor
    mkComputeType,

    -- * Lenses
    ctName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.Compute

-- | Describes the compute type.
--
-- /See:/ 'mkComputeType' smart constructor.
newtype ComputeType = ComputeType' {name :: Lude.Maybe Compute}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeType' with the minimum fields required to make a request.
--
-- * 'name' - The compute type.
mkComputeType ::
  ComputeType
mkComputeType = ComputeType' {name = Lude.Nothing}

-- | The compute type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' ComputeType (Lude.Maybe Compute)
ctName = Lens.lens (name :: ComputeType -> Lude.Maybe Compute) (\s a -> s {name = a} :: ComputeType)
{-# DEPRECATED ctName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ComputeType where
  parseJSON =
    Lude.withObject
      "ComputeType"
      (\x -> ComputeType' Lude.<$> (x Lude..:? "Name"))
