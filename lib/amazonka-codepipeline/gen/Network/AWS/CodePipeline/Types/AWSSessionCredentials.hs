{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.AWSSessionCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.AWSSessionCredentials
  ( AWSSessionCredentials (..)
  -- * Smart constructor
  , mkAWSSessionCredentials
  -- * Lenses
  , awsscAccessKeyId
  , awsscSecretAccessKey
  , awsscSessionToken
  ) where

import qualified Network.AWS.CodePipeline.Types.AccessKeyId as Types
import qualified Network.AWS.CodePipeline.Types.SecretAccessKey as Types
import qualified Network.AWS.CodePipeline.Types.SessionToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifact for the pipeline in AWS CodePipeline.
--
-- /See:/ 'mkAWSSessionCredentials' smart constructor.
data AWSSessionCredentials = AWSSessionCredentials'
  { accessKeyId :: Types.AccessKeyId
    -- ^ The access key for the session.
  , secretAccessKey :: Types.SecretAccessKey
    -- ^ The secret access key for the session.
  , sessionToken :: Types.SessionToken
    -- ^ The token for the session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AWSSessionCredentials' value with any optional fields omitted.
mkAWSSessionCredentials
    :: Types.AccessKeyId -- ^ 'accessKeyId'
    -> Types.SecretAccessKey -- ^ 'secretAccessKey'
    -> Types.SessionToken -- ^ 'sessionToken'
    -> AWSSessionCredentials
mkAWSSessionCredentials accessKeyId secretAccessKey sessionToken
  = AWSSessionCredentials'{accessKeyId, secretAccessKey,
                           sessionToken}

-- | The access key for the session.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsscAccessKeyId :: Lens.Lens' AWSSessionCredentials Types.AccessKeyId
awsscAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE awsscAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

-- | The secret access key for the session.
--
-- /Note:/ Consider using 'secretAccessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsscSecretAccessKey :: Lens.Lens' AWSSessionCredentials Types.SecretAccessKey
awsscSecretAccessKey = Lens.field @"secretAccessKey"
{-# INLINEABLE awsscSecretAccessKey #-}
{-# DEPRECATED secretAccessKey "Use generic-lens or generic-optics with 'secretAccessKey' instead"  #-}

-- | The token for the session.
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsscSessionToken :: Lens.Lens' AWSSessionCredentials Types.SessionToken
awsscSessionToken = Lens.field @"sessionToken"
{-# INLINEABLE awsscSessionToken #-}
{-# DEPRECATED sessionToken "Use generic-lens or generic-optics with 'sessionToken' instead"  #-}

instance Core.FromJSON AWSSessionCredentials where
        parseJSON
          = Core.withObject "AWSSessionCredentials" Core.$
              \ x ->
                AWSSessionCredentials' Core.<$>
                  (x Core..: "accessKeyId") Core.<*> x Core..: "secretAccessKey"
                    Core.<*> x Core..: "sessionToken"
