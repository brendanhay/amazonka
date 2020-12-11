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
    edMessage,
    edCode,
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
  { message :: Lude.Text,
    code :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorDetails' with the minimum fields required to make a request.
--
-- * 'code' - The error code associated with the operation failure.
-- * 'message' - Detailed information about the reason that the operation failed.
mkErrorDetails ::
  -- | 'message'
  Lude.Text ->
  -- | 'code'
  Lude.Text ->
  ErrorDetails
mkErrorDetails pMessage_ pCode_ =
  ErrorDetails' {message = pMessage_, code = pCode_}

-- | Detailed information about the reason that the operation failed.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' ErrorDetails Lude.Text
edMessage = Lens.lens (message :: ErrorDetails -> Lude.Text) (\s a -> s {message = a} :: ErrorDetails)
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The error code associated with the operation failure.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCode :: Lens.Lens' ErrorDetails Lude.Text
edCode = Lens.lens (code :: ErrorDetails -> Lude.Text) (\s a -> s {code = a} :: ErrorDetails)
{-# DEPRECATED edCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromXML ErrorDetails where
  parseXML x =
    ErrorDetails'
      Lude.<$> (x Lude..@ "Message") Lude.<*> (x Lude..@ "Code")
