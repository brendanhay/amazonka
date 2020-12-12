{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationError
  ( ValidationError (..),

    -- * Smart constructor
    mkValidationError,

    -- * Lenses
    veCode,
    veMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The error code and error message that is returned for a parameter or parameter combination that is not valid when a new launch template or new version of a launch template is created.
--
-- /See:/ 'mkValidationError' smart constructor.
data ValidationError = ValidationError'
  { code ::
      Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- * 'code' - The error code that indicates why the parameter or parameter combination is not valid. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
-- * 'message' - The error message that describes why the parameter or parameter combination is not valid. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
mkValidationError ::
  ValidationError
mkValidationError =
  ValidationError' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code that indicates why the parameter or parameter combination is not valid. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veCode :: Lens.Lens' ValidationError (Lude.Maybe Lude.Text)
veCode = Lens.lens (code :: ValidationError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: ValidationError)
{-# DEPRECATED veCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message that describes why the parameter or parameter combination is not valid. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veMessage :: Lens.Lens' ValidationError (Lude.Maybe Lude.Text)
veMessage = Lens.lens (message :: ValidationError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ValidationError)
{-# DEPRECATED veMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ValidationError where
  parseXML x =
    ValidationError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
