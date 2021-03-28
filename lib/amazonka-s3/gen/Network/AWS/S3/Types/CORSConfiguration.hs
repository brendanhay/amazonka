{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CORSConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.CORSConfiguration
  ( CORSConfiguration (..)
  -- * Smart constructor
  , mkCORSConfiguration
  -- * Lenses
  , corscCORSRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.CORSRule as Types

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkCORSConfiguration' smart constructor.
newtype CORSConfiguration = CORSConfiguration'
  { cORSRules :: [Types.CORSRule]
    -- ^ A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CORSConfiguration' value with any optional fields omitted.
mkCORSConfiguration
    :: CORSConfiguration
mkCORSConfiguration = CORSConfiguration'{cORSRules = Core.mempty}

-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
--
-- /Note:/ Consider using 'cORSRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corscCORSRules :: Lens.Lens' CORSConfiguration [Types.CORSRule]
corscCORSRules = Lens.field @"cORSRules"
{-# INLINEABLE corscCORSRules #-}
{-# DEPRECATED cORSRules "Use generic-lens or generic-optics with 'cORSRules' instead"  #-}

instance Core.ToXML CORSConfiguration where
        toXML CORSConfiguration{..} = Core.toXMLList "CORSRule" cORSRules
