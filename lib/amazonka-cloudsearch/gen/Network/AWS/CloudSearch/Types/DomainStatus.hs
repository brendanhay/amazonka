{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainStatus
  ( DomainStatus (..),

    -- * Smart constructor
    mkDomainStatus,

    -- * Lenses
    dsDomainId,
    dsDomainName,
    dsRequiresIndexDocuments,
    dsARN,
    dsCreated,
    dsDeleted,
    dsDocService,
    dsLimits,
    dsProcessing,
    dsSearchInstanceCount,
    dsSearchInstanceType,
    dsSearchPartitionCount,
    dsSearchService,
  )
where

import qualified Network.AWS.CloudSearch.Types.ARN as Types
import qualified Network.AWS.CloudSearch.Types.DomainId as Types
import qualified Network.AWS.CloudSearch.Types.DomainName as Types
import qualified Network.AWS.CloudSearch.Types.Limits as Types
import qualified Network.AWS.CloudSearch.Types.SearchInstanceType as Types
import qualified Network.AWS.CloudSearch.Types.ServiceEndpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current status of the search domain.
--
-- /See:/ 'mkDomainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { domainId :: Types.DomainId,
    domainName :: Types.DomainName,
    -- | True if 'IndexDocuments' needs to be called to activate the current domain configuration.
    requiresIndexDocuments :: Core.Bool,
    arn :: Core.Maybe Types.ARN,
    -- | True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
    created :: Core.Maybe Core.Bool,
    -- | True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
    deleted :: Core.Maybe Core.Bool,
    -- | The service endpoint for updating documents in a search domain.
    docService :: Core.Maybe Types.ServiceEndpoint,
    limits :: Core.Maybe Types.Limits,
    -- | True if processing is being done to activate the current domain configuration.
    processing :: Core.Maybe Core.Bool,
    -- | The number of search instances that are available to process search requests.
    searchInstanceCount :: Core.Maybe Core.Natural,
    -- | The instance type that is being used to process search requests.
    searchInstanceType :: Core.Maybe Types.SearchInstanceType,
    -- | The number of partitions across which the search index is spread.
    searchPartitionCount :: Core.Maybe Core.Natural,
    -- | The service endpoint for requesting search results from a search domain.
    searchService :: Core.Maybe Types.ServiceEndpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainStatus' value with any optional fields omitted.
mkDomainStatus ::
  -- | 'domainId'
  Types.DomainId ->
  -- | 'domainName'
  Types.DomainName ->
  -- | 'requiresIndexDocuments'
  Core.Bool ->
  DomainStatus
mkDomainStatus domainId domainName requiresIndexDocuments =
  DomainStatus'
    { domainId,
      domainName,
      requiresIndexDocuments,
      arn = Core.Nothing,
      created = Core.Nothing,
      deleted = Core.Nothing,
      docService = Core.Nothing,
      limits = Core.Nothing,
      processing = Core.Nothing,
      searchInstanceCount = Core.Nothing,
      searchInstanceType = Core.Nothing,
      searchPartitionCount = Core.Nothing,
      searchService = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainId :: Lens.Lens' DomainStatus Types.DomainId
dsDomainId = Lens.field @"domainId"
{-# DEPRECATED dsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DomainStatus Types.DomainName
dsDomainName = Lens.field @"domainName"
{-# DEPRECATED dsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | True if 'IndexDocuments' needs to be called to activate the current domain configuration.
--
-- /Note:/ Consider using 'requiresIndexDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRequiresIndexDocuments :: Lens.Lens' DomainStatus Core.Bool
dsRequiresIndexDocuments = Lens.field @"requiresIndexDocuments"
{-# DEPRECATED dsRequiresIndexDocuments "Use generic-lens or generic-optics with 'requiresIndexDocuments' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsARN :: Lens.Lens' DomainStatus (Core.Maybe Types.ARN)
dsARN = Lens.field @"arn"
{-# DEPRECATED dsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreated :: Lens.Lens' DomainStatus (Core.Maybe Core.Bool)
dsCreated = Lens.field @"created"
{-# DEPRECATED dsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
--
-- /Note:/ Consider using 'deleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDeleted :: Lens.Lens' DomainStatus (Core.Maybe Core.Bool)
dsDeleted = Lens.field @"deleted"
{-# DEPRECATED dsDeleted "Use generic-lens or generic-optics with 'deleted' instead." #-}

-- | The service endpoint for updating documents in a search domain.
--
-- /Note:/ Consider using 'docService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDocService :: Lens.Lens' DomainStatus (Core.Maybe Types.ServiceEndpoint)
dsDocService = Lens.field @"docService"
{-# DEPRECATED dsDocService "Use generic-lens or generic-optics with 'docService' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimits :: Lens.Lens' DomainStatus (Core.Maybe Types.Limits)
dsLimits = Lens.field @"limits"
{-# DEPRECATED dsLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | True if processing is being done to activate the current domain configuration.
--
-- /Note:/ Consider using 'processing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsProcessing :: Lens.Lens' DomainStatus (Core.Maybe Core.Bool)
dsProcessing = Lens.field @"processing"
{-# DEPRECATED dsProcessing "Use generic-lens or generic-optics with 'processing' instead." #-}

-- | The number of search instances that are available to process search requests.
--
-- /Note:/ Consider using 'searchInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchInstanceCount :: Lens.Lens' DomainStatus (Core.Maybe Core.Natural)
dsSearchInstanceCount = Lens.field @"searchInstanceCount"
{-# DEPRECATED dsSearchInstanceCount "Use generic-lens or generic-optics with 'searchInstanceCount' instead." #-}

-- | The instance type that is being used to process search requests.
--
-- /Note:/ Consider using 'searchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchInstanceType :: Lens.Lens' DomainStatus (Core.Maybe Types.SearchInstanceType)
dsSearchInstanceType = Lens.field @"searchInstanceType"
{-# DEPRECATED dsSearchInstanceType "Use generic-lens or generic-optics with 'searchInstanceType' instead." #-}

-- | The number of partitions across which the search index is spread.
--
-- /Note:/ Consider using 'searchPartitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchPartitionCount :: Lens.Lens' DomainStatus (Core.Maybe Core.Natural)
dsSearchPartitionCount = Lens.field @"searchPartitionCount"
{-# DEPRECATED dsSearchPartitionCount "Use generic-lens or generic-optics with 'searchPartitionCount' instead." #-}

-- | The service endpoint for requesting search results from a search domain.
--
-- /Note:/ Consider using 'searchService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchService :: Lens.Lens' DomainStatus (Core.Maybe Types.ServiceEndpoint)
dsSearchService = Lens.field @"searchService"
{-# DEPRECATED dsSearchService "Use generic-lens or generic-optics with 'searchService' instead." #-}

instance Core.FromXML DomainStatus where
  parseXML x =
    DomainStatus'
      Core.<$> (x Core..@ "DomainId")
      Core.<*> (x Core..@ "DomainName")
      Core.<*> (x Core..@ "RequiresIndexDocuments")
      Core.<*> (x Core..@? "ARN")
      Core.<*> (x Core..@? "Created")
      Core.<*> (x Core..@? "Deleted")
      Core.<*> (x Core..@? "DocService")
      Core.<*> (x Core..@? "Limits")
      Core.<*> (x Core..@? "Processing")
      Core.<*> (x Core..@? "SearchInstanceCount")
      Core.<*> (x Core..@? "SearchInstanceType")
      Core.<*> (x Core..@? "SearchPartitionCount")
      Core.<*> (x Core..@? "SearchService")
