{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DynamoDbSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.DynamoDbSettings
  ( DynamoDbSettings (..)
  -- * Smart constructor
  , mkDynamoDbSettings
  -- * Lenses
  , ddsServiceAccessRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role used to define an Amazon DynamoDB target endpoint.
--
-- /See:/ 'mkDynamoDbSettings' smart constructor.
newtype DynamoDbSettings = DynamoDbSettings'
  { serviceAccessRoleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) used by the service access IAM role. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DynamoDbSettings' value with any optional fields omitted.
mkDynamoDbSettings
    :: Core.Text -- ^ 'serviceAccessRoleArn'
    -> DynamoDbSettings
mkDynamoDbSettings serviceAccessRoleArn
  = DynamoDbSettings'{serviceAccessRoleArn}

-- | The Amazon Resource Name (ARN) used by the service access IAM role. 
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsServiceAccessRoleArn :: Lens.Lens' DynamoDbSettings Core.Text
ddsServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# INLINEABLE ddsServiceAccessRoleArn #-}
{-# DEPRECATED serviceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead"  #-}

instance Core.FromJSON DynamoDbSettings where
        toJSON DynamoDbSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServiceAccessRoleArn" Core..= serviceAccessRoleArn)])

instance Core.FromJSON DynamoDbSettings where
        parseJSON
          = Core.withObject "DynamoDbSettings" Core.$
              \ x ->
                DynamoDbSettings' Core.<$> (x Core..: "ServiceAccessRoleArn")
