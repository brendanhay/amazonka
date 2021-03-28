{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SigV4Authorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.SigV4Authorization
  ( SigV4Authorization (..)
  -- * Smart constructor
  , mkSigV4Authorization
  -- * Lenses
  , svaSigningRegion
  , svaServiceName
  , svaRoleArn
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.ServiceName as Types
import qualified Network.AWS.IoT.Types.SigningRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 signing process> .
--
-- /See:/ 'mkSigV4Authorization' smart constructor.
data SigV4Authorization = SigV4Authorization'
  { signingRegion :: Types.SigningRegion
    -- ^ The signing region.
  , serviceName :: Types.ServiceName
    -- ^ The service name to use while signing with Sig V4.
  , roleArn :: Types.AwsArn
    -- ^ The ARN of the signing role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SigV4Authorization' value with any optional fields omitted.
mkSigV4Authorization
    :: Types.SigningRegion -- ^ 'signingRegion'
    -> Types.ServiceName -- ^ 'serviceName'
    -> Types.AwsArn -- ^ 'roleArn'
    -> SigV4Authorization
mkSigV4Authorization signingRegion serviceName roleArn
  = SigV4Authorization'{signingRegion, serviceName, roleArn}

-- | The signing region.
--
-- /Note:/ Consider using 'signingRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svaSigningRegion :: Lens.Lens' SigV4Authorization Types.SigningRegion
svaSigningRegion = Lens.field @"signingRegion"
{-# INLINEABLE svaSigningRegion #-}
{-# DEPRECATED signingRegion "Use generic-lens or generic-optics with 'signingRegion' instead"  #-}

-- | The service name to use while signing with Sig V4.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svaServiceName :: Lens.Lens' SigV4Authorization Types.ServiceName
svaServiceName = Lens.field @"serviceName"
{-# INLINEABLE svaServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The ARN of the signing role.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svaRoleArn :: Lens.Lens' SigV4Authorization Types.AwsArn
svaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE svaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON SigV4Authorization where
        toJSON SigV4Authorization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("signingRegion" Core..= signingRegion),
                  Core.Just ("serviceName" Core..= serviceName),
                  Core.Just ("roleArn" Core..= roleArn)])

instance Core.FromJSON SigV4Authorization where
        parseJSON
          = Core.withObject "SigV4Authorization" Core.$
              \ x ->
                SigV4Authorization' Core.<$>
                  (x Core..: "signingRegion") Core.<*> x Core..: "serviceName"
                    Core.<*> x Core..: "roleArn"
