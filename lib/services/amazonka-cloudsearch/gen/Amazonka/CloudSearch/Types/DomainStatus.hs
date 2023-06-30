{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearch.Types.DomainStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DomainStatus where

import Amazonka.CloudSearch.Types.Limits
import Amazonka.CloudSearch.Types.ServiceEndpoint
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of the search domain.
--
-- /See:/ 'newDomainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { arn :: Prelude.Maybe Prelude.Text,
    -- | True if the search domain is created. It can take several minutes to
    -- initialize a domain when CreateDomain is called. Newly created search
    -- domains are returned from DescribeDomains with a false value for Created
    -- until domain creation is complete.
    created :: Prelude.Maybe Prelude.Bool,
    -- | True if the search domain has been deleted. The system must clean up
    -- resources dedicated to the search domain when DeleteDomain is called.
    -- Newly deleted search domains are returned from DescribeDomains with a
    -- true value for IsDeleted for several minutes until resource cleanup is
    -- complete.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The service endpoint for updating documents in a search domain.
    docService :: Prelude.Maybe ServiceEndpoint,
    limits :: Prelude.Maybe Limits,
    -- | True if processing is being done to activate the current domain
    -- configuration.
    processing :: Prelude.Maybe Prelude.Bool,
    -- | The number of search instances that are available to process search
    -- requests.
    searchInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The instance type that is being used to process search requests.
    searchInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The number of partitions across which the search index is spread.
    searchPartitionCount :: Prelude.Maybe Prelude.Natural,
    -- | The service endpoint for requesting search results from a search domain.
    searchService :: Prelude.Maybe ServiceEndpoint,
    domainId :: Prelude.Text,
    domainName :: Prelude.Text,
    -- | True if IndexDocuments needs to be called to activate the current domain
    -- configuration.
    requiresIndexDocuments :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'domainStatus_arn' - Undocumented member.
--
-- 'created', 'domainStatus_created' - True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
--
-- 'deleted', 'domainStatus_deleted' - True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called.
-- Newly deleted search domains are returned from DescribeDomains with a
-- true value for IsDeleted for several minutes until resource cleanup is
-- complete.
--
-- 'docService', 'domainStatus_docService' - The service endpoint for updating documents in a search domain.
--
-- 'limits', 'domainStatus_limits' - Undocumented member.
--
-- 'processing', 'domainStatus_processing' - True if processing is being done to activate the current domain
-- configuration.
--
-- 'searchInstanceCount', 'domainStatus_searchInstanceCount' - The number of search instances that are available to process search
-- requests.
--
-- 'searchInstanceType', 'domainStatus_searchInstanceType' - The instance type that is being used to process search requests.
--
-- 'searchPartitionCount', 'domainStatus_searchPartitionCount' - The number of partitions across which the search index is spread.
--
-- 'searchService', 'domainStatus_searchService' - The service endpoint for requesting search results from a search domain.
--
-- 'domainId', 'domainStatus_domainId' - Undocumented member.
--
-- 'domainName', 'domainStatus_domainName' - Undocumented member.
--
-- 'requiresIndexDocuments', 'domainStatus_requiresIndexDocuments' - True if IndexDocuments needs to be called to activate the current domain
-- configuration.
newDomainStatus ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'requiresIndexDocuments'
  Prelude.Bool ->
  DomainStatus
newDomainStatus
  pDomainId_
  pDomainName_
  pRequiresIndexDocuments_ =
    DomainStatus'
      { arn = Prelude.Nothing,
        created = Prelude.Nothing,
        deleted = Prelude.Nothing,
        docService = Prelude.Nothing,
        limits = Prelude.Nothing,
        processing = Prelude.Nothing,
        searchInstanceCount = Prelude.Nothing,
        searchInstanceType = Prelude.Nothing,
        searchPartitionCount = Prelude.Nothing,
        searchService = Prelude.Nothing,
        domainId = pDomainId_,
        domainName = pDomainName_,
        requiresIndexDocuments = pRequiresIndexDocuments_
      }

-- | Undocumented member.
domainStatus_arn :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_arn = Lens.lens (\DomainStatus' {arn} -> arn) (\s@DomainStatus' {} a -> s {arn = a} :: DomainStatus)

-- | True if the search domain is created. It can take several minutes to
-- initialize a domain when CreateDomain is called. Newly created search
-- domains are returned from DescribeDomains with a false value for Created
-- until domain creation is complete.
domainStatus_created :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_created = Lens.lens (\DomainStatus' {created} -> created) (\s@DomainStatus' {} a -> s {created = a} :: DomainStatus)

-- | True if the search domain has been deleted. The system must clean up
-- resources dedicated to the search domain when DeleteDomain is called.
-- Newly deleted search domains are returned from DescribeDomains with a
-- true value for IsDeleted for several minutes until resource cleanup is
-- complete.
domainStatus_deleted :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_deleted = Lens.lens (\DomainStatus' {deleted} -> deleted) (\s@DomainStatus' {} a -> s {deleted = a} :: DomainStatus)

-- | The service endpoint for updating documents in a search domain.
domainStatus_docService :: Lens.Lens' DomainStatus (Prelude.Maybe ServiceEndpoint)
domainStatus_docService = Lens.lens (\DomainStatus' {docService} -> docService) (\s@DomainStatus' {} a -> s {docService = a} :: DomainStatus)

-- | Undocumented member.
domainStatus_limits :: Lens.Lens' DomainStatus (Prelude.Maybe Limits)
domainStatus_limits = Lens.lens (\DomainStatus' {limits} -> limits) (\s@DomainStatus' {} a -> s {limits = a} :: DomainStatus)

-- | True if processing is being done to activate the current domain
-- configuration.
domainStatus_processing :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Bool)
domainStatus_processing = Lens.lens (\DomainStatus' {processing} -> processing) (\s@DomainStatus' {} a -> s {processing = a} :: DomainStatus)

-- | The number of search instances that are available to process search
-- requests.
domainStatus_searchInstanceCount :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Natural)
domainStatus_searchInstanceCount = Lens.lens (\DomainStatus' {searchInstanceCount} -> searchInstanceCount) (\s@DomainStatus' {} a -> s {searchInstanceCount = a} :: DomainStatus)

-- | The instance type that is being used to process search requests.
domainStatus_searchInstanceType :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Text)
domainStatus_searchInstanceType = Lens.lens (\DomainStatus' {searchInstanceType} -> searchInstanceType) (\s@DomainStatus' {} a -> s {searchInstanceType = a} :: DomainStatus)

-- | The number of partitions across which the search index is spread.
domainStatus_searchPartitionCount :: Lens.Lens' DomainStatus (Prelude.Maybe Prelude.Natural)
domainStatus_searchPartitionCount = Lens.lens (\DomainStatus' {searchPartitionCount} -> searchPartitionCount) (\s@DomainStatus' {} a -> s {searchPartitionCount = a} :: DomainStatus)

-- | The service endpoint for requesting search results from a search domain.
domainStatus_searchService :: Lens.Lens' DomainStatus (Prelude.Maybe ServiceEndpoint)
domainStatus_searchService = Lens.lens (\DomainStatus' {searchService} -> searchService) (\s@DomainStatus' {} a -> s {searchService = a} :: DomainStatus)

-- | Undocumented member.
domainStatus_domainId :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_domainId = Lens.lens (\DomainStatus' {domainId} -> domainId) (\s@DomainStatus' {} a -> s {domainId = a} :: DomainStatus)

-- | Undocumented member.
domainStatus_domainName :: Lens.Lens' DomainStatus Prelude.Text
domainStatus_domainName = Lens.lens (\DomainStatus' {domainName} -> domainName) (\s@DomainStatus' {} a -> s {domainName = a} :: DomainStatus)

-- | True if IndexDocuments needs to be called to activate the current domain
-- configuration.
domainStatus_requiresIndexDocuments :: Lens.Lens' DomainStatus Prelude.Bool
domainStatus_requiresIndexDocuments = Lens.lens (\DomainStatus' {requiresIndexDocuments} -> requiresIndexDocuments) (\s@DomainStatus' {} a -> s {requiresIndexDocuments = a} :: DomainStatus)

instance Data.FromXML DomainStatus where
  parseXML x =
    DomainStatus'
      Prelude.<$> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "Created")
      Prelude.<*> (x Data..@? "Deleted")
      Prelude.<*> (x Data..@? "DocService")
      Prelude.<*> (x Data..@? "Limits")
      Prelude.<*> (x Data..@? "Processing")
      Prelude.<*> (x Data..@? "SearchInstanceCount")
      Prelude.<*> (x Data..@? "SearchInstanceType")
      Prelude.<*> (x Data..@? "SearchPartitionCount")
      Prelude.<*> (x Data..@? "SearchService")
      Prelude.<*> (x Data..@ "DomainId")
      Prelude.<*> (x Data..@ "DomainName")
      Prelude.<*> (x Data..@ "RequiresIndexDocuments")

instance Prelude.Hashable DomainStatus where
  hashWithSalt _salt DomainStatus' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` deleted
      `Prelude.hashWithSalt` docService
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` processing
      `Prelude.hashWithSalt` searchInstanceCount
      `Prelude.hashWithSalt` searchInstanceType
      `Prelude.hashWithSalt` searchPartitionCount
      `Prelude.hashWithSalt` searchService
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` requiresIndexDocuments

instance Prelude.NFData DomainStatus where
  rnf DomainStatus' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf deleted
      `Prelude.seq` Prelude.rnf docService
      `Prelude.seq` Prelude.rnf limits
      `Prelude.seq` Prelude.rnf processing
      `Prelude.seq` Prelude.rnf searchInstanceCount
      `Prelude.seq` Prelude.rnf searchInstanceType
      `Prelude.seq` Prelude.rnf searchPartitionCount
      `Prelude.seq` Prelude.rnf searchService
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf requiresIndexDocuments
