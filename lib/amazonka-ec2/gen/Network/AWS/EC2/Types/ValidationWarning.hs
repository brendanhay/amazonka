{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ValidationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationWarning
  ( ValidationWarning (..),

    -- * Smart constructor
    mkValidationWarning,

    -- * Lenses
    vwErrors,
  )
where

import Network.AWS.EC2.Types.ValidationError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The error codes and error messages that are returned for the parameters or parameter combinations that are not valid when a new launch template or new version of a launch template is created.
--
-- /See:/ 'mkValidationWarning' smart constructor.
newtype ValidationWarning = ValidationWarning'
  { errors ::
      Lude.Maybe [ValidationError]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationWarning' with the minimum fields required to make a request.
--
-- * 'errors' - The error codes and error messages.
mkValidationWarning ::
  ValidationWarning
mkValidationWarning = ValidationWarning' {errors = Lude.Nothing}

-- | The error codes and error messages.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwErrors :: Lens.Lens' ValidationWarning (Lude.Maybe [ValidationError])
vwErrors = Lens.lens (errors :: ValidationWarning -> Lude.Maybe [ValidationError]) (\s a -> s {errors = a} :: ValidationWarning)
{-# DEPRECATED vwErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

instance Lude.FromXML ValidationWarning where
  parseXML x =
    ValidationWarning'
      Lude.<$> ( x Lude..@? "errorSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
