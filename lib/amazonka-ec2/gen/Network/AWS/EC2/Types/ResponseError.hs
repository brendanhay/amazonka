{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseError
  ( ResponseError (..),

    -- * Smart constructor
    mkResponseError,

    -- * Lenses
    reCode,
    reMessage,
  )
where

import Network.AWS.EC2.Types.LaunchTemplateErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the error that's returned when you cannot delete a launch template version.
--
-- /See:/ 'mkResponseError' smart constructor.
data ResponseError = ResponseError'
  { -- | The error code.
    code :: Lude.Maybe LaunchTemplateErrorCode,
    -- | The error message, if applicable.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResponseError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message, if applicable.
mkResponseError ::
  ResponseError
mkResponseError =
  ResponseError' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCode :: Lens.Lens' ResponseError (Lude.Maybe LaunchTemplateErrorCode)
reCode = Lens.lens (code :: ResponseError -> Lude.Maybe LaunchTemplateErrorCode) (\s a -> s {code = a} :: ResponseError)
{-# DEPRECATED reCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reMessage :: Lens.Lens' ResponseError (Lude.Maybe Lude.Text)
reMessage = Lens.lens (message :: ResponseError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ResponseError)
{-# DEPRECATED reMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ResponseError where
  parseXML x =
    ResponseError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
