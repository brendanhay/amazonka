-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ContextDataType
  ( ContextDataType (..),

    -- * Smart constructor
    mkContextDataType,

    -- * Lenses
    cdtEncodedData,
    cdtIPAddress,
    cdtServerName,
    cdtServerPath,
    cdtHTTPHeaders,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /See:/ 'mkContextDataType' smart constructor.
data ContextDataType = ContextDataType'
  { encodedData ::
      Lude.Maybe Lude.Text,
    ipAddress :: Lude.Text,
    serverName :: Lude.Text,
    serverPath :: Lude.Text,
    hTTPHeaders :: [HTTPHeader]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContextDataType' with the minimum fields required to make a request.
--
-- * 'encodedData' - Encoded data containing device fingerprinting details, collected using the Amazon Cognito context data collection library.
-- * 'hTTPHeaders' - HttpHeaders received on your server in same order.
-- * 'ipAddress' - Source IP address of your user.
-- * 'serverName' - Your server endpoint where this API is invoked.
-- * 'serverPath' - Your server path where this API is invoked.
mkContextDataType ::
  -- | 'ipAddress'
  Lude.Text ->
  -- | 'serverName'
  Lude.Text ->
  -- | 'serverPath'
  Lude.Text ->
  ContextDataType
mkContextDataType pIPAddress_ pServerName_ pServerPath_ =
  ContextDataType'
    { encodedData = Lude.Nothing,
      ipAddress = pIPAddress_,
      serverName = pServerName_,
      serverPath = pServerPath_,
      hTTPHeaders = Lude.mempty
    }

-- | Encoded data containing device fingerprinting details, collected using the Amazon Cognito context data collection library.
--
-- /Note:/ Consider using 'encodedData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtEncodedData :: Lens.Lens' ContextDataType (Lude.Maybe Lude.Text)
cdtEncodedData = Lens.lens (encodedData :: ContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {encodedData = a} :: ContextDataType)
{-# DEPRECATED cdtEncodedData "Use generic-lens or generic-optics with 'encodedData' instead." #-}

-- | Source IP address of your user.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtIPAddress :: Lens.Lens' ContextDataType Lude.Text
cdtIPAddress = Lens.lens (ipAddress :: ContextDataType -> Lude.Text) (\s a -> s {ipAddress = a} :: ContextDataType)
{-# DEPRECATED cdtIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Your server endpoint where this API is invoked.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtServerName :: Lens.Lens' ContextDataType Lude.Text
cdtServerName = Lens.lens (serverName :: ContextDataType -> Lude.Text) (\s a -> s {serverName = a} :: ContextDataType)
{-# DEPRECATED cdtServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Your server path where this API is invoked.
--
-- /Note:/ Consider using 'serverPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtServerPath :: Lens.Lens' ContextDataType Lude.Text
cdtServerPath = Lens.lens (serverPath :: ContextDataType -> Lude.Text) (\s a -> s {serverPath = a} :: ContextDataType)
{-# DEPRECATED cdtServerPath "Use generic-lens or generic-optics with 'serverPath' instead." #-}

-- | HttpHeaders received on your server in same order.
--
-- /Note:/ Consider using 'hTTPHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtHTTPHeaders :: Lens.Lens' ContextDataType [HTTPHeader]
cdtHTTPHeaders = Lens.lens (hTTPHeaders :: ContextDataType -> [HTTPHeader]) (\s a -> s {hTTPHeaders = a} :: ContextDataType)
{-# DEPRECATED cdtHTTPHeaders "Use generic-lens or generic-optics with 'hTTPHeaders' instead." #-}

instance Lude.ToJSON ContextDataType where
  toJSON ContextDataType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EncodedData" Lude..=) Lude.<$> encodedData,
            Lude.Just ("IpAddress" Lude..= ipAddress),
            Lude.Just ("ServerName" Lude..= serverName),
            Lude.Just ("ServerPath" Lude..= serverPath),
            Lude.Just ("HttpHeaders" Lude..= hTTPHeaders)
          ]
      )
