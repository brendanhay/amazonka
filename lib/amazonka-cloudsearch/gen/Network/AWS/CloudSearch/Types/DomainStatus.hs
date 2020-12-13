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
    dsSearchInstanceCount,
    dsSearchInstanceType,
    dsDocService,
    dsARN,
    dsCreated,
    dsSearchService,
    dsLimits,
    dsRequiresIndexDocuments,
    dsDomainName,
    dsSearchPartitionCount,
    dsDeleted,
    dsDomainId,
    dsProcessing,
  )
where

import Network.AWS.CloudSearch.Types.Limits
import Network.AWS.CloudSearch.Types.ServiceEndpoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current status of the search domain.
--
-- /See:/ 'mkDomainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { -- | The number of search instances that are available to process search requests.
    searchInstanceCount :: Lude.Maybe Lude.Natural,
    -- | The instance type that is being used to process search requests.
    searchInstanceType :: Lude.Maybe Lude.Text,
    -- | The service endpoint for updating documents in a search domain.
    docService :: Lude.Maybe ServiceEndpoint,
    arn :: Lude.Maybe Lude.Text,
    -- | True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
    created :: Lude.Maybe Lude.Bool,
    -- | The service endpoint for requesting search results from a search domain.
    searchService :: Lude.Maybe ServiceEndpoint,
    limits :: Lude.Maybe Limits,
    -- | True if 'IndexDocuments' needs to be called to activate the current domain configuration.
    requiresIndexDocuments :: Lude.Bool,
    domainName :: Lude.Text,
    -- | The number of partitions across which the search index is spread.
    searchPartitionCount :: Lude.Maybe Lude.Natural,
    -- | True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
    deleted :: Lude.Maybe Lude.Bool,
    domainId :: Lude.Text,
    -- | True if processing is being done to activate the current domain configuration.
    processing :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainStatus' with the minimum fields required to make a request.
--
-- * 'searchInstanceCount' - The number of search instances that are available to process search requests.
-- * 'searchInstanceType' - The instance type that is being used to process search requests.
-- * 'docService' - The service endpoint for updating documents in a search domain.
-- * 'arn' -
-- * 'created' - True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
-- * 'searchService' - The service endpoint for requesting search results from a search domain.
-- * 'limits' -
-- * 'requiresIndexDocuments' - True if 'IndexDocuments' needs to be called to activate the current domain configuration.
-- * 'domainName' -
-- * 'searchPartitionCount' - The number of partitions across which the search index is spread.
-- * 'deleted' - True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
-- * 'domainId' -
-- * 'processing' - True if processing is being done to activate the current domain configuration.
mkDomainStatus ::
  -- | 'requiresIndexDocuments'
  Lude.Bool ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'domainId'
  Lude.Text ->
  DomainStatus
mkDomainStatus pRequiresIndexDocuments_ pDomainName_ pDomainId_ =
  DomainStatus'
    { searchInstanceCount = Lude.Nothing,
      searchInstanceType = Lude.Nothing,
      docService = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      searchService = Lude.Nothing,
      limits = Lude.Nothing,
      requiresIndexDocuments = pRequiresIndexDocuments_,
      domainName = pDomainName_,
      searchPartitionCount = Lude.Nothing,
      deleted = Lude.Nothing,
      domainId = pDomainId_,
      processing = Lude.Nothing
    }

-- | The number of search instances that are available to process search requests.
--
-- /Note:/ Consider using 'searchInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchInstanceCount :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Natural)
dsSearchInstanceCount = Lens.lens (searchInstanceCount :: DomainStatus -> Lude.Maybe Lude.Natural) (\s a -> s {searchInstanceCount = a} :: DomainStatus)
{-# DEPRECATED dsSearchInstanceCount "Use generic-lens or generic-optics with 'searchInstanceCount' instead." #-}

-- | The instance type that is being used to process search requests.
--
-- /Note:/ Consider using 'searchInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchInstanceType :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Text)
dsSearchInstanceType = Lens.lens (searchInstanceType :: DomainStatus -> Lude.Maybe Lude.Text) (\s a -> s {searchInstanceType = a} :: DomainStatus)
{-# DEPRECATED dsSearchInstanceType "Use generic-lens or generic-optics with 'searchInstanceType' instead." #-}

-- | The service endpoint for updating documents in a search domain.
--
-- /Note:/ Consider using 'docService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDocService :: Lens.Lens' DomainStatus (Lude.Maybe ServiceEndpoint)
dsDocService = Lens.lens (docService :: DomainStatus -> Lude.Maybe ServiceEndpoint) (\s a -> s {docService = a} :: DomainStatus)
{-# DEPRECATED dsDocService "Use generic-lens or generic-optics with 'docService' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsARN :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Text)
dsARN = Lens.lens (arn :: DomainStatus -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DomainStatus)
{-# DEPRECATED dsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreated :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Bool)
dsCreated = Lens.lens (created :: DomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {created = a} :: DomainStatus)
{-# DEPRECATED dsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The service endpoint for requesting search results from a search domain.
--
-- /Note:/ Consider using 'searchService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchService :: Lens.Lens' DomainStatus (Lude.Maybe ServiceEndpoint)
dsSearchService = Lens.lens (searchService :: DomainStatus -> Lude.Maybe ServiceEndpoint) (\s a -> s {searchService = a} :: DomainStatus)
{-# DEPRECATED dsSearchService "Use generic-lens or generic-optics with 'searchService' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimits :: Lens.Lens' DomainStatus (Lude.Maybe Limits)
dsLimits = Lens.lens (limits :: DomainStatus -> Lude.Maybe Limits) (\s a -> s {limits = a} :: DomainStatus)
{-# DEPRECATED dsLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | True if 'IndexDocuments' needs to be called to activate the current domain configuration.
--
-- /Note:/ Consider using 'requiresIndexDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRequiresIndexDocuments :: Lens.Lens' DomainStatus Lude.Bool
dsRequiresIndexDocuments = Lens.lens (requiresIndexDocuments :: DomainStatus -> Lude.Bool) (\s a -> s {requiresIndexDocuments = a} :: DomainStatus)
{-# DEPRECATED dsRequiresIndexDocuments "Use generic-lens or generic-optics with 'requiresIndexDocuments' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DomainStatus Lude.Text
dsDomainName = Lens.lens (domainName :: DomainStatus -> Lude.Text) (\s a -> s {domainName = a} :: DomainStatus)
{-# DEPRECATED dsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The number of partitions across which the search index is spread.
--
-- /Note:/ Consider using 'searchPartitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSearchPartitionCount :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Natural)
dsSearchPartitionCount = Lens.lens (searchPartitionCount :: DomainStatus -> Lude.Maybe Lude.Natural) (\s a -> s {searchPartitionCount = a} :: DomainStatus)
{-# DEPRECATED dsSearchPartitionCount "Use generic-lens or generic-optics with 'searchPartitionCount' instead." #-}

-- | True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
--
-- /Note:/ Consider using 'deleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDeleted :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Bool)
dsDeleted = Lens.lens (deleted :: DomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {deleted = a} :: DomainStatus)
{-# DEPRECATED dsDeleted "Use generic-lens or generic-optics with 'deleted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainId :: Lens.Lens' DomainStatus Lude.Text
dsDomainId = Lens.lens (domainId :: DomainStatus -> Lude.Text) (\s a -> s {domainId = a} :: DomainStatus)
{-# DEPRECATED dsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | True if processing is being done to activate the current domain configuration.
--
-- /Note:/ Consider using 'processing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsProcessing :: Lens.Lens' DomainStatus (Lude.Maybe Lude.Bool)
dsProcessing = Lens.lens (processing :: DomainStatus -> Lude.Maybe Lude.Bool) (\s a -> s {processing = a} :: DomainStatus)
{-# DEPRECATED dsProcessing "Use generic-lens or generic-optics with 'processing' instead." #-}

instance Lude.FromXML DomainStatus where
  parseXML x =
    DomainStatus'
      Lude.<$> (x Lude..@? "SearchInstanceCount")
      Lude.<*> (x Lude..@? "SearchInstanceType")
      Lude.<*> (x Lude..@? "DocService")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "Created")
      Lude.<*> (x Lude..@? "SearchService")
      Lude.<*> (x Lude..@? "Limits")
      Lude.<*> (x Lude..@ "RequiresIndexDocuments")
      Lude.<*> (x Lude..@ "DomainName")
      Lude.<*> (x Lude..@? "SearchPartitionCount")
      Lude.<*> (x Lude..@? "Deleted")
      Lude.<*> (x Lude..@ "DomainId")
      Lude.<*> (x Lude..@? "Processing")
