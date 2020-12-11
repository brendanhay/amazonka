-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
  ( UserContextDataType (..),

    -- * Smart constructor
    mkUserContextDataType,

    -- * Lenses
    ucdtEncodedData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /See:/ 'mkUserContextDataType' smart constructor.
newtype UserContextDataType = UserContextDataType'
  { encodedData ::
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

-- | Creates a value of 'UserContextDataType' with the minimum fields required to make a request.
--
-- * 'encodedData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
mkUserContextDataType ::
  UserContextDataType
mkUserContextDataType =
  UserContextDataType' {encodedData = Lude.Nothing}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'encodedData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdtEncodedData :: Lens.Lens' UserContextDataType (Lude.Maybe Lude.Text)
ucdtEncodedData = Lens.lens (encodedData :: UserContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {encodedData = a} :: UserContextDataType)
{-# DEPRECATED ucdtEncodedData "Use generic-lens or generic-optics with 'encodedData' instead." #-}

instance Lude.ToJSON UserContextDataType where
  toJSON UserContextDataType' {..} =
    Lude.object
      (Lude.catMaybes [("EncodedData" Lude..=) Lude.<$> encodedData])
