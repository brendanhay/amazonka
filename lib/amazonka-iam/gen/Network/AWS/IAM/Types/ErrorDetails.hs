{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    edCode,
    edMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the reason that the operation failed.
--
-- This data type is used as a response element in the 'GetOrganizationsAccessReport' , 'GetServiceLastAccessedDetails' , and 'GetServiceLastAccessedDetailsWithEntities' operations.
--
-- /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | The error code associated with the operation failure.
    code :: Lude.Text,
    -- | Detailed information about the reason that the operation failed.
    message :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorDetails' with the minimum fields required to make a request.
--
-- * 'code' - The error code associated with the operation failure.
-- * 'message' - Detailed information about the reason that the operation failed.
mkErrorDetails ::
  -- | 'code'
  Lude.Text ->
  -- | 'message'
  Lude.Text ->
  ErrorDetails
mkErrorDetails pCode_ pMessage_ =
  ErrorDetails' {code = pCode_, message = pMessage_}

-- | The error code associated with the operation failure.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCode :: Lens.Lens' ErrorDetails Lude.Text
edCode = Lens.lens (code :: ErrorDetails -> Lude.Text) (\s a -> s {code = a} :: ErrorDetails)
{-# DEPRECATED edCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | Detailed information about the reason that the operation failed.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' ErrorDetails Lude.Text
edMessage = Lens.lens (message :: ErrorDetails -> Lude.Text) (\s a -> s {message = a} :: ErrorDetails)
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ErrorDetails where
  parseXML x =
    ErrorDetails'
      Lude.<$> (x Lude..@ "Code") Lude.<*> (x Lude..@ "Message")
