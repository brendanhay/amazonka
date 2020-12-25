{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainDetails
  ( DomainDetails (..),

    -- * Smart constructor
    mkDomainDetails,

    -- * Lenses
    ddCreationTime,
    ddDomainArn,
    ddDomainId,
    ddDomainName,
    ddLastModifiedTime,
    ddStatus,
    ddUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DomainArn as Types
import qualified Network.AWS.SageMaker.Types.DomainId as Types
import qualified Network.AWS.SageMaker.Types.DomainName as Types
import qualified Network.AWS.SageMaker.Types.DomainStatus as Types
import qualified Network.AWS.SageMaker.Types.Url as Types

-- | The domain's details.
--
-- /See:/ 'mkDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The creation time.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The domain's Amazon Resource Name (ARN).
    domainArn :: Core.Maybe Types.DomainArn,
    -- | The domain ID.
    domainId :: Core.Maybe Types.DomainId,
    -- | The domain name.
    domainName :: Core.Maybe Types.DomainName,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status.
    status :: Core.Maybe Types.DomainStatus,
    -- | The domain's URL.
    url :: Core.Maybe Types.Url
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DomainDetails' value with any optional fields omitted.
mkDomainDetails ::
  DomainDetails
mkDomainDetails =
  DomainDetails'
    { creationTime = Core.Nothing,
      domainArn = Core.Nothing,
      domainId = Core.Nothing,
      domainName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      status = Core.Nothing,
      url = Core.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCreationTime :: Lens.Lens' DomainDetails (Core.Maybe Core.NominalDiffTime)
ddCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ddCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The domain's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'domainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainArn :: Lens.Lens' DomainDetails (Core.Maybe Types.DomainArn)
ddDomainArn = Lens.field @"domainArn"
{-# DEPRECATED ddDomainArn "Use generic-lens or generic-optics with 'domainArn' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainId :: Lens.Lens' DomainDetails (Core.Maybe Types.DomainId)
ddDomainId = Lens.field @"domainId"
{-# DEPRECATED ddDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DomainDetails (Core.Maybe Types.DomainName)
ddDomainName = Lens.field @"domainName"
{-# DEPRECATED ddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLastModifiedTime :: Lens.Lens' DomainDetails (Core.Maybe Core.NominalDiffTime)
ddLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED ddLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStatus :: Lens.Lens' DomainDetails (Core.Maybe Types.DomainStatus)
ddStatus = Lens.field @"status"
{-# DEPRECATED ddStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The domain's URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddUrl :: Lens.Lens' DomainDetails (Core.Maybe Types.Url)
ddUrl = Lens.field @"url"
{-# DEPRECATED ddUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON DomainDetails where
  parseJSON =
    Core.withObject "DomainDetails" Core.$
      \x ->
        DomainDetails'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DomainArn")
          Core.<*> (x Core..:? "DomainId")
          Core.<*> (x Core..:? "DomainName")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Url")
