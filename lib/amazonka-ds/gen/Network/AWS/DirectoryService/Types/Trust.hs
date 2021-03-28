{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Trust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Trust
  ( Trust (..)
  -- * Smart constructor
  , mkTrust
  -- * Lenses
  , tCreatedDateTime
  , tDirectoryId
  , tLastUpdatedDateTime
  , tRemoteDomainName
  , tSelectiveAuth
  , tStateLastUpdatedDateTime
  , tTrustDirection
  , tTrustId
  , tTrustState
  , tTrustStateReason
  , tTrustType
  ) where

import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.RemoteDomainName as Types
import qualified Network.AWS.DirectoryService.Types.SelectiveAuth as Types
import qualified Network.AWS.DirectoryService.Types.TrustDirection as Types
import qualified Network.AWS.DirectoryService.Types.TrustId as Types
import qualified Network.AWS.DirectoryService.Types.TrustState as Types
import qualified Network.AWS.DirectoryService.Types.TrustStateReason as Types
import qualified Network.AWS.DirectoryService.Types.TrustType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkTrust' smart constructor.
data Trust = Trust'
  { createdDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the trust relationship was created.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The Directory ID of the AWS directory involved in the trust relationship.
  , lastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the trust relationship was last updated.
  , remoteDomainName :: Core.Maybe Types.RemoteDomainName
    -- ^ The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
  , selectiveAuth :: Core.Maybe Types.SelectiveAuth
    -- ^ Current state of selective authentication for the trust.
  , stateLastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the TrustState was last updated.
  , trustDirection :: Core.Maybe Types.TrustDirection
    -- ^ The trust relationship direction.
  , trustId :: Core.Maybe Types.TrustId
    -- ^ The unique ID of the trust relationship.
  , trustState :: Core.Maybe Types.TrustState
    -- ^ The trust relationship state.
  , trustStateReason :: Core.Maybe Types.TrustStateReason
    -- ^ The reason for the TrustState.
  , trustType :: Core.Maybe Types.TrustType
    -- ^ The trust relationship type. @Forest@ is the default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Trust' value with any optional fields omitted.
mkTrust
    :: Trust
mkTrust
  = Trust'{createdDateTime = Core.Nothing,
           directoryId = Core.Nothing, lastUpdatedDateTime = Core.Nothing,
           remoteDomainName = Core.Nothing, selectiveAuth = Core.Nothing,
           stateLastUpdatedDateTime = Core.Nothing,
           trustDirection = Core.Nothing, trustId = Core.Nothing,
           trustState = Core.Nothing, trustStateReason = Core.Nothing,
           trustType = Core.Nothing}

-- | The date and time that the trust relationship was created.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedDateTime :: Lens.Lens' Trust (Core.Maybe Core.NominalDiffTime)
tCreatedDateTime = Lens.field @"createdDateTime"
{-# INLINEABLE tCreatedDateTime #-}
{-# DEPRECATED createdDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead"  #-}

-- | The Directory ID of the AWS directory involved in the trust relationship.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDirectoryId :: Lens.Lens' Trust (Core.Maybe Types.DirectoryId)
tDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE tDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The date and time that the trust relationship was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastUpdatedDateTime :: Lens.Lens' Trust (Core.Maybe Core.NominalDiffTime)
tLastUpdatedDateTime = Lens.field @"lastUpdatedDateTime"
{-# INLINEABLE tLastUpdatedDateTime #-}
{-# DEPRECATED lastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead"  #-}

-- | The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRemoteDomainName :: Lens.Lens' Trust (Core.Maybe Types.RemoteDomainName)
tRemoteDomainName = Lens.field @"remoteDomainName"
{-# INLINEABLE tRemoteDomainName #-}
{-# DEPRECATED remoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead"  #-}

-- | Current state of selective authentication for the trust.
--
-- /Note:/ Consider using 'selectiveAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSelectiveAuth :: Lens.Lens' Trust (Core.Maybe Types.SelectiveAuth)
tSelectiveAuth = Lens.field @"selectiveAuth"
{-# INLINEABLE tSelectiveAuth #-}
{-# DEPRECATED selectiveAuth "Use generic-lens or generic-optics with 'selectiveAuth' instead"  #-}

-- | The date and time that the TrustState was last updated.
--
-- /Note:/ Consider using 'stateLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStateLastUpdatedDateTime :: Lens.Lens' Trust (Core.Maybe Core.NominalDiffTime)
tStateLastUpdatedDateTime = Lens.field @"stateLastUpdatedDateTime"
{-# INLINEABLE tStateLastUpdatedDateTime #-}
{-# DEPRECATED stateLastUpdatedDateTime "Use generic-lens or generic-optics with 'stateLastUpdatedDateTime' instead"  #-}

-- | The trust relationship direction.
--
-- /Note:/ Consider using 'trustDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustDirection :: Lens.Lens' Trust (Core.Maybe Types.TrustDirection)
tTrustDirection = Lens.field @"trustDirection"
{-# INLINEABLE tTrustDirection #-}
{-# DEPRECATED trustDirection "Use generic-lens or generic-optics with 'trustDirection' instead"  #-}

-- | The unique ID of the trust relationship.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustId :: Lens.Lens' Trust (Core.Maybe Types.TrustId)
tTrustId = Lens.field @"trustId"
{-# INLINEABLE tTrustId #-}
{-# DEPRECATED trustId "Use generic-lens or generic-optics with 'trustId' instead"  #-}

-- | The trust relationship state.
--
-- /Note:/ Consider using 'trustState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustState :: Lens.Lens' Trust (Core.Maybe Types.TrustState)
tTrustState = Lens.field @"trustState"
{-# INLINEABLE tTrustState #-}
{-# DEPRECATED trustState "Use generic-lens or generic-optics with 'trustState' instead"  #-}

-- | The reason for the TrustState.
--
-- /Note:/ Consider using 'trustStateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustStateReason :: Lens.Lens' Trust (Core.Maybe Types.TrustStateReason)
tTrustStateReason = Lens.field @"trustStateReason"
{-# INLINEABLE tTrustStateReason #-}
{-# DEPRECATED trustStateReason "Use generic-lens or generic-optics with 'trustStateReason' instead"  #-}

-- | The trust relationship type. @Forest@ is the default.
--
-- /Note:/ Consider using 'trustType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustType :: Lens.Lens' Trust (Core.Maybe Types.TrustType)
tTrustType = Lens.field @"trustType"
{-# INLINEABLE tTrustType #-}
{-# DEPRECATED trustType "Use generic-lens or generic-optics with 'trustType' instead"  #-}

instance Core.FromJSON Trust where
        parseJSON
          = Core.withObject "Trust" Core.$
              \ x ->
                Trust' Core.<$>
                  (x Core..:? "CreatedDateTime") Core.<*> x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "LastUpdatedDateTime"
                    Core.<*> x Core..:? "RemoteDomainName"
                    Core.<*> x Core..:? "SelectiveAuth"
                    Core.<*> x Core..:? "StateLastUpdatedDateTime"
                    Core.<*> x Core..:? "TrustDirection"
                    Core.<*> x Core..:? "TrustId"
                    Core.<*> x Core..:? "TrustState"
                    Core.<*> x Core..:? "TrustStateReason"
                    Core.<*> x Core..:? "TrustType"
