{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.FileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.FileShareInfo
  ( FileShareInfo (..)
  -- * Smart constructor
  , mkFileShareInfo
  -- * Lenses
  , fsiFileShareARN
  , fsiFileShareId
  , fsiFileShareStatus
  , fsiFileShareType
  , fsiGatewayARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.FileShareARN as Types
import qualified Network.AWS.StorageGateway.Types.FileShareId as Types
import qualified Network.AWS.StorageGateway.Types.FileShareStatus as Types
import qualified Network.AWS.StorageGateway.Types.FileShareType as Types
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types

-- | Describes a file share.
--
-- /See:/ 'mkFileShareInfo' smart constructor.
data FileShareInfo = FileShareInfo'
  { fileShareARN :: Core.Maybe Types.FileShareARN
  , fileShareId :: Core.Maybe Types.FileShareId
  , fileShareStatus :: Core.Maybe Types.FileShareStatus
  , fileShareType :: Core.Maybe Types.FileShareType
  , gatewayARN :: Core.Maybe Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileShareInfo' value with any optional fields omitted.
mkFileShareInfo
    :: FileShareInfo
mkFileShareInfo
  = FileShareInfo'{fileShareARN = Core.Nothing,
                   fileShareId = Core.Nothing, fileShareStatus = Core.Nothing,
                   fileShareType = Core.Nothing, gatewayARN = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareARN :: Lens.Lens' FileShareInfo (Core.Maybe Types.FileShareARN)
fsiFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE fsiFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareId :: Lens.Lens' FileShareInfo (Core.Maybe Types.FileShareId)
fsiFileShareId = Lens.field @"fileShareId"
{-# INLINEABLE fsiFileShareId #-}
{-# DEPRECATED fileShareId "Use generic-lens or generic-optics with 'fileShareId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareStatus :: Lens.Lens' FileShareInfo (Core.Maybe Types.FileShareStatus)
fsiFileShareStatus = Lens.field @"fileShareStatus"
{-# INLINEABLE fsiFileShareStatus #-}
{-# DEPRECATED fileShareStatus "Use generic-lens or generic-optics with 'fileShareStatus' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiFileShareType :: Lens.Lens' FileShareInfo (Core.Maybe Types.FileShareType)
fsiFileShareType = Lens.field @"fileShareType"
{-# INLINEABLE fsiFileShareType #-}
{-# DEPRECATED fileShareType "Use generic-lens or generic-optics with 'fileShareType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsiGatewayARN :: Lens.Lens' FileShareInfo (Core.Maybe Types.GatewayARN)
fsiGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE fsiGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.FromJSON FileShareInfo where
        parseJSON
          = Core.withObject "FileShareInfo" Core.$
              \ x ->
                FileShareInfo' Core.<$>
                  (x Core..:? "FileShareARN") Core.<*> x Core..:? "FileShareId"
                    Core.<*> x Core..:? "FileShareStatus"
                    Core.<*> x Core..:? "FileShareType"
                    Core.<*> x Core..:? "GatewayARN"
