{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AwsCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.AwsCredentials
  ( AwsCredentials (..)
  -- * Smart constructor
  , mkAwsCredentials
  -- * Lenses
  , acAccessKeyId
  , acSecretAccessKey
  , acSessionToken
  ) where

import qualified Network.AWS.GameLift.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Temporary access credentials used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling 'RequestUploadCredentials' .
--
-- /See:/ 'mkAwsCredentials' smart constructor.
data AwsCredentials = AwsCredentials'
  { accessKeyId :: Core.Maybe Types.NonEmptyString
    -- ^ Temporary key allowing access to the Amazon GameLift S3 account.
  , secretAccessKey :: Core.Maybe Types.NonEmptyString
    -- ^ Temporary secret key allowing access to the Amazon GameLift S3 account.
  , sessionToken :: Core.Maybe Types.NonEmptyString
    -- ^ Token used to associate a specific build ID with the files uploaded using these credentials.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsCredentials' value with any optional fields omitted.
mkAwsCredentials
    :: AwsCredentials
mkAwsCredentials
  = AwsCredentials'{accessKeyId = Core.Nothing,
                    secretAccessKey = Core.Nothing, sessionToken = Core.Nothing}

-- | Temporary key allowing access to the Amazon GameLift S3 account.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAccessKeyId :: Lens.Lens' AwsCredentials (Core.Maybe Types.NonEmptyString)
acAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE acAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

-- | Temporary secret key allowing access to the Amazon GameLift S3 account.
--
-- /Note:/ Consider using 'secretAccessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSecretAccessKey :: Lens.Lens' AwsCredentials (Core.Maybe Types.NonEmptyString)
acSecretAccessKey = Lens.field @"secretAccessKey"
{-# INLINEABLE acSecretAccessKey #-}
{-# DEPRECATED secretAccessKey "Use generic-lens or generic-optics with 'secretAccessKey' instead"  #-}

-- | Token used to associate a specific build ID with the files uploaded using these credentials.
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSessionToken :: Lens.Lens' AwsCredentials (Core.Maybe Types.NonEmptyString)
acSessionToken = Lens.field @"sessionToken"
{-# INLINEABLE acSessionToken #-}
{-# DEPRECATED sessionToken "Use generic-lens or generic-optics with 'sessionToken' instead"  #-}

instance Core.FromJSON AwsCredentials where
        parseJSON
          = Core.withObject "AwsCredentials" Core.$
              \ x ->
                AwsCredentials' Core.<$>
                  (x Core..:? "AccessKeyId") Core.<*> x Core..:? "SecretAccessKey"
                    Core.<*> x Core..:? "SessionToken"
