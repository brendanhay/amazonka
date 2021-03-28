{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
  ( GroupCertificateAuthorityProperties (..)
  -- * Smart constructor
  , mkGroupCertificateAuthorityProperties
  -- * Lenses
  , gcapGroupCertificateAuthorityArn
  , gcapGroupCertificateAuthorityId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a certificate authority for a group.
--
-- /See:/ 'mkGroupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
  { groupCertificateAuthorityArn :: Core.Maybe Core.Text
    -- ^ The ARN of the certificate authority for the group.
  , groupCertificateAuthorityId :: Core.Maybe Core.Text
    -- ^ The ID of the certificate authority for the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupCertificateAuthorityProperties' value with any optional fields omitted.
mkGroupCertificateAuthorityProperties
    :: GroupCertificateAuthorityProperties
mkGroupCertificateAuthorityProperties
  = GroupCertificateAuthorityProperties'{groupCertificateAuthorityArn
                                           = Core.Nothing,
                                         groupCertificateAuthorityId = Core.Nothing}

-- | The ARN of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapGroupCertificateAuthorityArn :: Lens.Lens' GroupCertificateAuthorityProperties (Core.Maybe Core.Text)
gcapGroupCertificateAuthorityArn = Lens.field @"groupCertificateAuthorityArn"
{-# INLINEABLE gcapGroupCertificateAuthorityArn #-}
{-# DEPRECATED groupCertificateAuthorityArn "Use generic-lens or generic-optics with 'groupCertificateAuthorityArn' instead"  #-}

-- | The ID of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapGroupCertificateAuthorityId :: Lens.Lens' GroupCertificateAuthorityProperties (Core.Maybe Core.Text)
gcapGroupCertificateAuthorityId = Lens.field @"groupCertificateAuthorityId"
{-# INLINEABLE gcapGroupCertificateAuthorityId #-}
{-# DEPRECATED groupCertificateAuthorityId "Use generic-lens or generic-optics with 'groupCertificateAuthorityId' instead"  #-}

instance Core.FromJSON GroupCertificateAuthorityProperties where
        parseJSON
          = Core.withObject "GroupCertificateAuthorityProperties" Core.$
              \ x ->
                GroupCertificateAuthorityProperties' Core.<$>
                  (x Core..:? "GroupCertificateAuthorityArn") Core.<*>
                    x Core..:? "GroupCertificateAuthorityId"
