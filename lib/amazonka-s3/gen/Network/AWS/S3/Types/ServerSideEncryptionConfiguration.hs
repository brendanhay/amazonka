{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ServerSideEncryptionConfiguration
  ( ServerSideEncryptionConfiguration (..)
  -- * Smart constructor
  , mkServerSideEncryptionConfiguration
  -- * Lenses
  , ssecRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ServerSideEncryptionRule as Types

-- | Specifies the default server-side-encryption configuration.
--
-- /See:/ 'mkServerSideEncryptionConfiguration' smart constructor.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { rules :: [Types.ServerSideEncryptionRule]
    -- ^ Container for information about a particular server-side encryption configuration rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ServerSideEncryptionConfiguration' value with any optional fields omitted.
mkServerSideEncryptionConfiguration
    :: ServerSideEncryptionConfiguration
mkServerSideEncryptionConfiguration
  = ServerSideEncryptionConfiguration'{rules = Core.mempty}

-- | Container for information about a particular server-side encryption configuration rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssecRules :: Lens.Lens' ServerSideEncryptionConfiguration [Types.ServerSideEncryptionRule]
ssecRules = Lens.field @"rules"
{-# INLINEABLE ssecRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.ToXML ServerSideEncryptionConfiguration where
        toXML ServerSideEncryptionConfiguration{..}
          = Core.toXMLList "Rule" rules

instance Core.FromXML ServerSideEncryptionConfiguration where
        parseXML x
          = ServerSideEncryptionConfiguration' Core.<$>
              (x Core..@ "Rule" Core..@! Core.mempty)
