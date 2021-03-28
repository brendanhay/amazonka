{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
  ( UserContextDataType (..)
  -- * Smart constructor
  , mkUserContextDataType
  -- * Lenses
  , ucdtEncodedData
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /See:/ 'mkUserContextDataType' smart constructor.
newtype UserContextDataType = UserContextDataType'
  { encodedData :: Core.Maybe Types.StringType
    -- ^ Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UserContextDataType' value with any optional fields omitted.
mkUserContextDataType
    :: UserContextDataType
mkUserContextDataType
  = UserContextDataType'{encodedData = Core.Nothing}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'encodedData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdtEncodedData :: Lens.Lens' UserContextDataType (Core.Maybe Types.StringType)
ucdtEncodedData = Lens.field @"encodedData"
{-# INLINEABLE ucdtEncodedData #-}
{-# DEPRECATED encodedData "Use generic-lens or generic-optics with 'encodedData' instead"  #-}

instance Core.FromJSON UserContextDataType where
        toJSON UserContextDataType{..}
          = Core.object
              (Core.catMaybes [("EncodedData" Core..=) Core.<$> encodedData])
