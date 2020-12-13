{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AWSAPICallAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AWSAPICallAction
  ( AWSAPICallAction (..),

    -- * Smart constructor
    mkAWSAPICallAction,

    -- * Lenses
    aacaRemoteIPDetails,
    aacaCallerType,
    aacaDomainDetails,
    aacaServiceName,
    aacaErrorCode,
    aacaAPI,
  )
where

import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the API action.
--
-- /See:/ 'mkAWSAPICallAction' smart constructor.
data AWSAPICallAction = AWSAPICallAction'
  { -- | The remote IP information of the connection that initiated the AWS API call.
    remoteIPDetails :: Lude.Maybe RemoteIPDetails,
    -- | The AWS API caller type.
    callerType :: Lude.Maybe Lude.Text,
    -- | The domain information for the AWS API call.
    domainDetails :: Lude.Maybe DomainDetails,
    -- | The AWS service name whose API was invoked.
    serviceName :: Lude.Maybe Lude.Text,
    -- | The error code of the failed AWS API action.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The AWS API name.
    api :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSAPICallAction' with the minimum fields required to make a request.
--
-- * 'remoteIPDetails' - The remote IP information of the connection that initiated the AWS API call.
-- * 'callerType' - The AWS API caller type.
-- * 'domainDetails' - The domain information for the AWS API call.
-- * 'serviceName' - The AWS service name whose API was invoked.
-- * 'errorCode' - The error code of the failed AWS API action.
-- * 'api' - The AWS API name.
mkAWSAPICallAction ::
  AWSAPICallAction
mkAWSAPICallAction =
  AWSAPICallAction'
    { remoteIPDetails = Lude.Nothing,
      callerType = Lude.Nothing,
      domainDetails = Lude.Nothing,
      serviceName = Lude.Nothing,
      errorCode = Lude.Nothing,
      api = Lude.Nothing
    }

-- | The remote IP information of the connection that initiated the AWS API call.
--
-- /Note:/ Consider using 'remoteIPDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaRemoteIPDetails :: Lens.Lens' AWSAPICallAction (Lude.Maybe RemoteIPDetails)
aacaRemoteIPDetails = Lens.lens (remoteIPDetails :: AWSAPICallAction -> Lude.Maybe RemoteIPDetails) (\s a -> s {remoteIPDetails = a} :: AWSAPICallAction)
{-# DEPRECATED aacaRemoteIPDetails "Use generic-lens or generic-optics with 'remoteIPDetails' instead." #-}

-- | The AWS API caller type.
--
-- /Note:/ Consider using 'callerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaCallerType :: Lens.Lens' AWSAPICallAction (Lude.Maybe Lude.Text)
aacaCallerType = Lens.lens (callerType :: AWSAPICallAction -> Lude.Maybe Lude.Text) (\s a -> s {callerType = a} :: AWSAPICallAction)
{-# DEPRECATED aacaCallerType "Use generic-lens or generic-optics with 'callerType' instead." #-}

-- | The domain information for the AWS API call.
--
-- /Note:/ Consider using 'domainDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaDomainDetails :: Lens.Lens' AWSAPICallAction (Lude.Maybe DomainDetails)
aacaDomainDetails = Lens.lens (domainDetails :: AWSAPICallAction -> Lude.Maybe DomainDetails) (\s a -> s {domainDetails = a} :: AWSAPICallAction)
{-# DEPRECATED aacaDomainDetails "Use generic-lens or generic-optics with 'domainDetails' instead." #-}

-- | The AWS service name whose API was invoked.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaServiceName :: Lens.Lens' AWSAPICallAction (Lude.Maybe Lude.Text)
aacaServiceName = Lens.lens (serviceName :: AWSAPICallAction -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: AWSAPICallAction)
{-# DEPRECATED aacaServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The error code of the failed AWS API action.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaErrorCode :: Lens.Lens' AWSAPICallAction (Lude.Maybe Lude.Text)
aacaErrorCode = Lens.lens (errorCode :: AWSAPICallAction -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: AWSAPICallAction)
{-# DEPRECATED aacaErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The AWS API name.
--
-- /Note:/ Consider using 'api' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaAPI :: Lens.Lens' AWSAPICallAction (Lude.Maybe Lude.Text)
aacaAPI = Lens.lens (api :: AWSAPICallAction -> Lude.Maybe Lude.Text) (\s a -> s {api = a} :: AWSAPICallAction)
{-# DEPRECATED aacaAPI "Use generic-lens or generic-optics with 'api' instead." #-}

instance Lude.FromJSON AWSAPICallAction where
  parseJSON =
    Lude.withObject
      "AWSAPICallAction"
      ( \x ->
          AWSAPICallAction'
            Lude.<$> (x Lude..:? "remoteIpDetails")
            Lude.<*> (x Lude..:? "callerType")
            Lude.<*> (x Lude..:? "domainDetails")
            Lude.<*> (x Lude..:? "serviceName")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "api")
      )
