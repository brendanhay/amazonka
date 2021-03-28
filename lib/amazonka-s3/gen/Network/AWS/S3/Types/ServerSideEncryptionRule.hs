{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ServerSideEncryptionRule
  ( ServerSideEncryptionRule (..)
  -- * Smart constructor
  , mkServerSideEncryptionRule
  -- * Lenses
  , sserApplyServerSideEncryptionByDefault
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ServerSideEncryptionByDefault as Types

-- | Specifies the default server-side encryption configuration.
--
-- /See:/ 'mkServerSideEncryptionRule' smart constructor.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule'
  { applyServerSideEncryptionByDefault :: Core.Maybe Types.ServerSideEncryptionByDefault
    -- ^ Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ServerSideEncryptionRule' value with any optional fields omitted.
mkServerSideEncryptionRule
    :: ServerSideEncryptionRule
mkServerSideEncryptionRule
  = ServerSideEncryptionRule'{applyServerSideEncryptionByDefault =
                                Core.Nothing}

-- | Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
--
-- /Note:/ Consider using 'applyServerSideEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserApplyServerSideEncryptionByDefault :: Lens.Lens' ServerSideEncryptionRule (Core.Maybe Types.ServerSideEncryptionByDefault)
sserApplyServerSideEncryptionByDefault = Lens.field @"applyServerSideEncryptionByDefault"
{-# INLINEABLE sserApplyServerSideEncryptionByDefault #-}
{-# DEPRECATED applyServerSideEncryptionByDefault "Use generic-lens or generic-optics with 'applyServerSideEncryptionByDefault' instead"  #-}

instance Core.ToXML ServerSideEncryptionRule where
        toXML ServerSideEncryptionRule{..}
          = Core.maybe Core.mempty
              (Core.toXMLElement "ApplyServerSideEncryptionByDefault")
              applyServerSideEncryptionByDefault

instance Core.FromXML ServerSideEncryptionRule where
        parseXML x
          = ServerSideEncryptionRule' Core.<$>
              (x Core..@? "ApplyServerSideEncryptionByDefault")
