{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KeyGroup
  ( KeyGroup (..)
  -- * Smart constructor
  , mkKeyGroup
  -- * Lenses
  , kgId
  , kgLastModifiedTime
  , kgKeyGroupConfig
  ) where

import qualified Network.AWS.CloudFront.Types.KeyGroupConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key group.
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- /See:/ 'mkKeyGroup' smart constructor.
data KeyGroup = KeyGroup'
  { id :: Core.Text
    -- ^ The identifier for the key group.
  , lastModifiedTime :: Core.UTCTime
    -- ^ The date and time when the key group was last modified.
  , keyGroupConfig :: Types.KeyGroupConfig
    -- ^ The key group configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'KeyGroup' value with any optional fields omitted.
mkKeyGroup
    :: Core.Text -- ^ 'id'
    -> Core.UTCTime -- ^ 'lastModifiedTime'
    -> Types.KeyGroupConfig -- ^ 'keyGroupConfig'
    -> KeyGroup
mkKeyGroup id lastModifiedTime keyGroupConfig
  = KeyGroup'{id, lastModifiedTime, keyGroupConfig}

-- | The identifier for the key group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgId :: Lens.Lens' KeyGroup Core.Text
kgId = Lens.field @"id"
{-# INLINEABLE kgId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The date and time when the key group was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgLastModifiedTime :: Lens.Lens' KeyGroup Core.UTCTime
kgLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE kgLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgKeyGroupConfig :: Lens.Lens' KeyGroup Types.KeyGroupConfig
kgKeyGroupConfig = Lens.field @"keyGroupConfig"
{-# INLINEABLE kgKeyGroupConfig #-}
{-# DEPRECATED keyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead"  #-}

instance Core.FromXML KeyGroup where
        parseXML x
          = KeyGroup' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "LastModifiedTime" Core.<*>
                x Core..@ "KeyGroupConfig"
