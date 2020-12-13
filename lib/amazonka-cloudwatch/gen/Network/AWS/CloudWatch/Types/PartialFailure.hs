{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.PartialFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.PartialFailure
  ( PartialFailure (..),

    -- * Smart constructor
    mkPartialFailure,

    -- * Lenses
    pfFailureResource,
    pfFailureCode,
    pfFailureDescription,
    pfExceptionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This array is empty if the API operation was successful for all the rules specified in the request. If the operation could not process one of the rules, the following data is returned for each of those rules.
--
-- /See:/ 'mkPartialFailure' smart constructor.
data PartialFailure = PartialFailure'
  { -- | The specified rule that could not be deleted.
    failureResource :: Lude.Maybe Lude.Text,
    -- | The code of the error.
    failureCode :: Lude.Maybe Lude.Text,
    -- | A description of the error.
    failureDescription :: Lude.Maybe Lude.Text,
    -- | The type of error.
    exceptionType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartialFailure' with the minimum fields required to make a request.
--
-- * 'failureResource' - The specified rule that could not be deleted.
-- * 'failureCode' - The code of the error.
-- * 'failureDescription' - A description of the error.
-- * 'exceptionType' - The type of error.
mkPartialFailure ::
  PartialFailure
mkPartialFailure =
  PartialFailure'
    { failureResource = Lude.Nothing,
      failureCode = Lude.Nothing,
      failureDescription = Lude.Nothing,
      exceptionType = Lude.Nothing
    }

-- | The specified rule that could not be deleted.
--
-- /Note:/ Consider using 'failureResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFailureResource :: Lens.Lens' PartialFailure (Lude.Maybe Lude.Text)
pfFailureResource = Lens.lens (failureResource :: PartialFailure -> Lude.Maybe Lude.Text) (\s a -> s {failureResource = a} :: PartialFailure)
{-# DEPRECATED pfFailureResource "Use generic-lens or generic-optics with 'failureResource' instead." #-}

-- | The code of the error.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFailureCode :: Lens.Lens' PartialFailure (Lude.Maybe Lude.Text)
pfFailureCode = Lens.lens (failureCode :: PartialFailure -> Lude.Maybe Lude.Text) (\s a -> s {failureCode = a} :: PartialFailure)
{-# DEPRECATED pfFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | A description of the error.
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfFailureDescription :: Lens.Lens' PartialFailure (Lude.Maybe Lude.Text)
pfFailureDescription = Lens.lens (failureDescription :: PartialFailure -> Lude.Maybe Lude.Text) (\s a -> s {failureDescription = a} :: PartialFailure)
{-# DEPRECATED pfFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

-- | The type of error.
--
-- /Note:/ Consider using 'exceptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfExceptionType :: Lens.Lens' PartialFailure (Lude.Maybe Lude.Text)
pfExceptionType = Lens.lens (exceptionType :: PartialFailure -> Lude.Maybe Lude.Text) (\s a -> s {exceptionType = a} :: PartialFailure)
{-# DEPRECATED pfExceptionType "Use generic-lens or generic-optics with 'exceptionType' instead." #-}

instance Lude.FromXML PartialFailure where
  parseXML x =
    PartialFailure'
      Lude.<$> (x Lude..@? "FailureResource")
      Lude.<*> (x Lude..@? "FailureCode")
      Lude.<*> (x Lude..@? "FailureDescription")
      Lude.<*> (x Lude..@? "ExceptionType")
