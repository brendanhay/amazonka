{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedOperation
  ( SupportedOperation (..),

    -- * Smart constructor
    mkSupportedOperation,

    -- * Lenses
    soOperationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the operations that are allowed on a maintenance track.
--
-- /See:/ 'mkSupportedOperation' smart constructor.
newtype SupportedOperation = SupportedOperation'
  { operationName ::
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

-- | Creates a value of 'SupportedOperation' with the minimum fields required to make a request.
--
-- * 'operationName' - A list of the supported operations.
mkSupportedOperation ::
  SupportedOperation
mkSupportedOperation =
  SupportedOperation' {operationName = Lude.Nothing}

-- | A list of the supported operations.
--
-- /Note:/ Consider using 'operationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soOperationName :: Lens.Lens' SupportedOperation (Lude.Maybe Lude.Text)
soOperationName = Lens.lens (operationName :: SupportedOperation -> Lude.Maybe Lude.Text) (\s a -> s {operationName = a} :: SupportedOperation)
{-# DEPRECATED soOperationName "Use generic-lens or generic-optics with 'operationName' instead." #-}

instance Lude.FromXML SupportedOperation where
  parseXML x =
    SupportedOperation' Lude.<$> (x Lude..@? "OperationName")
