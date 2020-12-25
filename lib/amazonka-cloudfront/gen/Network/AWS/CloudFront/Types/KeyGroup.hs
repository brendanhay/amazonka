{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroup
  ( KeyGroup (..),

    -- * Smart constructor
    mkKeyGroup,

    -- * Lenses
    kgId,
    kgLastModifiedTime,
    kgKeyGroupConfig,
  )
where

import qualified Network.AWS.CloudFront.Types.KeyGroupConfig as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key group.
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- /See:/ 'mkKeyGroup' smart constructor.
data KeyGroup = KeyGroup'
  { -- | The identifier for the key group.
    id :: Types.String,
    -- | The date and time when the key group was last modified.
    lastModifiedTime :: Core.UTCTime,
    -- | The key group configuration.
    keyGroupConfig :: Types.KeyGroupConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'KeyGroup' value with any optional fields omitted.
mkKeyGroup ::
  -- | 'id'
  Types.String ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'keyGroupConfig'
  Types.KeyGroupConfig ->
  KeyGroup
mkKeyGroup id lastModifiedTime keyGroupConfig =
  KeyGroup' {id, lastModifiedTime, keyGroupConfig}

-- | The identifier for the key group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgId :: Lens.Lens' KeyGroup Types.String
kgId = Lens.field @"id"
{-# DEPRECATED kgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the key group was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgLastModifiedTime :: Lens.Lens' KeyGroup Core.UTCTime
kgLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED kgLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgKeyGroupConfig :: Lens.Lens' KeyGroup Types.KeyGroupConfig
kgKeyGroupConfig = Lens.field @"keyGroupConfig"
{-# DEPRECATED kgKeyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead." #-}

instance Core.FromXML KeyGroup where
  parseXML x =
    KeyGroup'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "KeyGroupConfig")
