{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DomainMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the domain, including when the domain was
-- created, the number of items and attributes in the domain, and the size
-- of the attribute names and values.
module Network.AWS.SDB.DomainMetadata
  ( -- * Creating a Request
    DomainMetadata (..),
    newDomainMetadata,

    -- * Request Lenses
    domainMetadata_domainName,

    -- * Destructuring the Response
    DomainMetadataResponse (..),
    newDomainMetadataResponse,

    -- * Response Lenses
    domainMetadataResponse_attributeNamesSizeBytes,
    domainMetadataResponse_attributeValueCount,
    domainMetadataResponse_attributeNameCount,
    domainMetadataResponse_attributeValuesSizeBytes,
    domainMetadataResponse_timestamp,
    domainMetadataResponse_itemCount,
    domainMetadataResponse_itemNamesSizeBytes,
    domainMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newDomainMetadata' smart constructor.
data DomainMetadata = DomainMetadata'
  { -- | The name of the domain for which to display the metadata of.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'domainMetadata_domainName' - The name of the domain for which to display the metadata of.
newDomainMetadata ::
  -- | 'domainName'
  Core.Text ->
  DomainMetadata
newDomainMetadata pDomainName_ =
  DomainMetadata' {domainName = pDomainName_}

-- | The name of the domain for which to display the metadata of.
domainMetadata_domainName :: Lens.Lens' DomainMetadata Core.Text
domainMetadata_domainName = Lens.lens (\DomainMetadata' {domainName} -> domainName) (\s@DomainMetadata' {} a -> s {domainName = a} :: DomainMetadata)

instance Core.AWSRequest DomainMetadata where
  type
    AWSResponse DomainMetadata =
      DomainMetadataResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DomainMetadataResult"
      ( \s h x ->
          DomainMetadataResponse'
            Core.<$> (x Core..@? "AttributeNamesSizeBytes")
            Core.<*> (x Core..@? "AttributeValueCount")
            Core.<*> (x Core..@? "AttributeNameCount")
            Core.<*> (x Core..@? "AttributeValuesSizeBytes")
            Core.<*> (x Core..@? "Timestamp")
            Core.<*> (x Core..@? "ItemCount")
            Core.<*> (x Core..@? "ItemNamesSizeBytes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DomainMetadata

instance Core.NFData DomainMetadata

instance Core.ToHeaders DomainMetadata where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DomainMetadata where
  toPath = Core.const "/"

instance Core.ToQuery DomainMetadata where
  toQuery DomainMetadata' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DomainMetadata" :: Core.ByteString),
        "Version" Core.=: ("2009-04-15" :: Core.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | /See:/ 'newDomainMetadataResponse' smart constructor.
data DomainMetadataResponse = DomainMetadataResponse'
  { -- | The total size of all unique attribute names in the domain, in bytes.
    attributeNamesSizeBytes :: Core.Maybe Core.Integer,
    -- | The number of all attribute name\/value pairs in the domain.
    attributeValueCount :: Core.Maybe Core.Int,
    -- | The number of unique attribute names in the domain.
    attributeNameCount :: Core.Maybe Core.Int,
    -- | The total size of all attribute values in the domain, in bytes.
    attributeValuesSizeBytes :: Core.Maybe Core.Integer,
    -- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
    timestamp :: Core.Maybe Core.Int,
    -- | The number of all items in the domain.
    itemCount :: Core.Maybe Core.Int,
    -- | The total size of all item names in the domain, in bytes.
    itemNamesSizeBytes :: Core.Maybe Core.Integer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNamesSizeBytes', 'domainMetadataResponse_attributeNamesSizeBytes' - The total size of all unique attribute names in the domain, in bytes.
--
-- 'attributeValueCount', 'domainMetadataResponse_attributeValueCount' - The number of all attribute name\/value pairs in the domain.
--
-- 'attributeNameCount', 'domainMetadataResponse_attributeNameCount' - The number of unique attribute names in the domain.
--
-- 'attributeValuesSizeBytes', 'domainMetadataResponse_attributeValuesSizeBytes' - The total size of all attribute values in the domain, in bytes.
--
-- 'timestamp', 'domainMetadataResponse_timestamp' - The data and time when metadata was calculated, in Epoch (UNIX) seconds.
--
-- 'itemCount', 'domainMetadataResponse_itemCount' - The number of all items in the domain.
--
-- 'itemNamesSizeBytes', 'domainMetadataResponse_itemNamesSizeBytes' - The total size of all item names in the domain, in bytes.
--
-- 'httpStatus', 'domainMetadataResponse_httpStatus' - The response's http status code.
newDomainMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DomainMetadataResponse
newDomainMetadataResponse pHttpStatus_ =
  DomainMetadataResponse'
    { attributeNamesSizeBytes =
        Core.Nothing,
      attributeValueCount = Core.Nothing,
      attributeNameCount = Core.Nothing,
      attributeValuesSizeBytes = Core.Nothing,
      timestamp = Core.Nothing,
      itemCount = Core.Nothing,
      itemNamesSizeBytes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total size of all unique attribute names in the domain, in bytes.
domainMetadataResponse_attributeNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
domainMetadataResponse_attributeNamesSizeBytes = Lens.lens (\DomainMetadataResponse' {attributeNamesSizeBytes} -> attributeNamesSizeBytes) (\s@DomainMetadataResponse' {} a -> s {attributeNamesSizeBytes = a} :: DomainMetadataResponse)

-- | The number of all attribute name\/value pairs in the domain.
domainMetadataResponse_attributeValueCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
domainMetadataResponse_attributeValueCount = Lens.lens (\DomainMetadataResponse' {attributeValueCount} -> attributeValueCount) (\s@DomainMetadataResponse' {} a -> s {attributeValueCount = a} :: DomainMetadataResponse)

-- | The number of unique attribute names in the domain.
domainMetadataResponse_attributeNameCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
domainMetadataResponse_attributeNameCount = Lens.lens (\DomainMetadataResponse' {attributeNameCount} -> attributeNameCount) (\s@DomainMetadataResponse' {} a -> s {attributeNameCount = a} :: DomainMetadataResponse)

-- | The total size of all attribute values in the domain, in bytes.
domainMetadataResponse_attributeValuesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
domainMetadataResponse_attributeValuesSizeBytes = Lens.lens (\DomainMetadataResponse' {attributeValuesSizeBytes} -> attributeValuesSizeBytes) (\s@DomainMetadataResponse' {} a -> s {attributeValuesSizeBytes = a} :: DomainMetadataResponse)

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
domainMetadataResponse_timestamp :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
domainMetadataResponse_timestamp = Lens.lens (\DomainMetadataResponse' {timestamp} -> timestamp) (\s@DomainMetadataResponse' {} a -> s {timestamp = a} :: DomainMetadataResponse)

-- | The number of all items in the domain.
domainMetadataResponse_itemCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
domainMetadataResponse_itemCount = Lens.lens (\DomainMetadataResponse' {itemCount} -> itemCount) (\s@DomainMetadataResponse' {} a -> s {itemCount = a} :: DomainMetadataResponse)

-- | The total size of all item names in the domain, in bytes.
domainMetadataResponse_itemNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
domainMetadataResponse_itemNamesSizeBytes = Lens.lens (\DomainMetadataResponse' {itemNamesSizeBytes} -> itemNamesSizeBytes) (\s@DomainMetadataResponse' {} a -> s {itemNamesSizeBytes = a} :: DomainMetadataResponse)

-- | The response's http status code.
domainMetadataResponse_httpStatus :: Lens.Lens' DomainMetadataResponse Core.Int
domainMetadataResponse_httpStatus = Lens.lens (\DomainMetadataResponse' {httpStatus} -> httpStatus) (\s@DomainMetadataResponse' {} a -> s {httpStatus = a} :: DomainMetadataResponse)

instance Core.NFData DomainMetadataResponse
