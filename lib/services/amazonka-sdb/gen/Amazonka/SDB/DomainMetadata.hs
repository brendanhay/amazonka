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
-- Module      : Amazonka.SDB.DomainMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the domain, including when the domain was
-- created, the number of items and attributes in the domain, and the size
-- of the attribute names and values.
module Amazonka.SDB.DomainMetadata
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
    domainMetadataResponse_itemCount,
    domainMetadataResponse_timestamp,
    domainMetadataResponse_attributeNameCount,
    domainMetadataResponse_attributeValueCount,
    domainMetadataResponse_itemNamesSizeBytes,
    domainMetadataResponse_attributeValuesSizeBytes,
    domainMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SDB.Types

-- | /See:/ 'newDomainMetadata' smart constructor.
data DomainMetadata = DomainMetadata'
  { -- | The name of the domain for which to display the metadata of.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DomainMetadata
newDomainMetadata pDomainName_ =
  DomainMetadata' {domainName = pDomainName_}

-- | The name of the domain for which to display the metadata of.
domainMetadata_domainName :: Lens.Lens' DomainMetadata Prelude.Text
domainMetadata_domainName = Lens.lens (\DomainMetadata' {domainName} -> domainName) (\s@DomainMetadata' {} a -> s {domainName = a} :: DomainMetadata)

instance Core.AWSRequest DomainMetadata where
  type
    AWSResponse DomainMetadata =
      DomainMetadataResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DomainMetadataResult"
      ( \s h x ->
          DomainMetadataResponse'
            Prelude.<$> (x Core..@? "AttributeNamesSizeBytes")
            Prelude.<*> (x Core..@? "ItemCount")
            Prelude.<*> (x Core..@? "Timestamp")
            Prelude.<*> (x Core..@? "AttributeNameCount")
            Prelude.<*> (x Core..@? "AttributeValueCount")
            Prelude.<*> (x Core..@? "ItemNamesSizeBytes")
            Prelude.<*> (x Core..@? "AttributeValuesSizeBytes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DomainMetadata where
  hashWithSalt _salt DomainMetadata' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DomainMetadata where
  rnf DomainMetadata' {..} = Prelude.rnf domainName

instance Core.ToHeaders DomainMetadata where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DomainMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery DomainMetadata where
  toQuery DomainMetadata' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DomainMetadata" :: Prelude.ByteString),
        "Version"
          Core.=: ("2009-04-15" :: Prelude.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | /See:/ 'newDomainMetadataResponse' smart constructor.
data DomainMetadataResponse = DomainMetadataResponse'
  { -- | The total size of all unique attribute names in the domain, in bytes.
    attributeNamesSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of all items in the domain.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
    timestamp :: Prelude.Maybe Prelude.Int,
    -- | The number of unique attribute names in the domain.
    attributeNameCount :: Prelude.Maybe Prelude.Int,
    -- | The number of all attribute name\/value pairs in the domain.
    attributeValueCount :: Prelude.Maybe Prelude.Int,
    -- | The total size of all item names in the domain, in bytes.
    itemNamesSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total size of all attribute values in the domain, in bytes.
    attributeValuesSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'itemCount', 'domainMetadataResponse_itemCount' - The number of all items in the domain.
--
-- 'timestamp', 'domainMetadataResponse_timestamp' - The data and time when metadata was calculated, in Epoch (UNIX) seconds.
--
-- 'attributeNameCount', 'domainMetadataResponse_attributeNameCount' - The number of unique attribute names in the domain.
--
-- 'attributeValueCount', 'domainMetadataResponse_attributeValueCount' - The number of all attribute name\/value pairs in the domain.
--
-- 'itemNamesSizeBytes', 'domainMetadataResponse_itemNamesSizeBytes' - The total size of all item names in the domain, in bytes.
--
-- 'attributeValuesSizeBytes', 'domainMetadataResponse_attributeValuesSizeBytes' - The total size of all attribute values in the domain, in bytes.
--
-- 'httpStatus', 'domainMetadataResponse_httpStatus' - The response's http status code.
newDomainMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DomainMetadataResponse
newDomainMetadataResponse pHttpStatus_ =
  DomainMetadataResponse'
    { attributeNamesSizeBytes =
        Prelude.Nothing,
      itemCount = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      attributeNameCount = Prelude.Nothing,
      attributeValueCount = Prelude.Nothing,
      itemNamesSizeBytes = Prelude.Nothing,
      attributeValuesSizeBytes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total size of all unique attribute names in the domain, in bytes.
domainMetadataResponse_attributeNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Integer)
domainMetadataResponse_attributeNamesSizeBytes = Lens.lens (\DomainMetadataResponse' {attributeNamesSizeBytes} -> attributeNamesSizeBytes) (\s@DomainMetadataResponse' {} a -> s {attributeNamesSizeBytes = a} :: DomainMetadataResponse)

-- | The number of all items in the domain.
domainMetadataResponse_itemCount :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Int)
domainMetadataResponse_itemCount = Lens.lens (\DomainMetadataResponse' {itemCount} -> itemCount) (\s@DomainMetadataResponse' {} a -> s {itemCount = a} :: DomainMetadataResponse)

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
domainMetadataResponse_timestamp :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Int)
domainMetadataResponse_timestamp = Lens.lens (\DomainMetadataResponse' {timestamp} -> timestamp) (\s@DomainMetadataResponse' {} a -> s {timestamp = a} :: DomainMetadataResponse)

-- | The number of unique attribute names in the domain.
domainMetadataResponse_attributeNameCount :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Int)
domainMetadataResponse_attributeNameCount = Lens.lens (\DomainMetadataResponse' {attributeNameCount} -> attributeNameCount) (\s@DomainMetadataResponse' {} a -> s {attributeNameCount = a} :: DomainMetadataResponse)

-- | The number of all attribute name\/value pairs in the domain.
domainMetadataResponse_attributeValueCount :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Int)
domainMetadataResponse_attributeValueCount = Lens.lens (\DomainMetadataResponse' {attributeValueCount} -> attributeValueCount) (\s@DomainMetadataResponse' {} a -> s {attributeValueCount = a} :: DomainMetadataResponse)

-- | The total size of all item names in the domain, in bytes.
domainMetadataResponse_itemNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Integer)
domainMetadataResponse_itemNamesSizeBytes = Lens.lens (\DomainMetadataResponse' {itemNamesSizeBytes} -> itemNamesSizeBytes) (\s@DomainMetadataResponse' {} a -> s {itemNamesSizeBytes = a} :: DomainMetadataResponse)

-- | The total size of all attribute values in the domain, in bytes.
domainMetadataResponse_attributeValuesSizeBytes :: Lens.Lens' DomainMetadataResponse (Prelude.Maybe Prelude.Integer)
domainMetadataResponse_attributeValuesSizeBytes = Lens.lens (\DomainMetadataResponse' {attributeValuesSizeBytes} -> attributeValuesSizeBytes) (\s@DomainMetadataResponse' {} a -> s {attributeValuesSizeBytes = a} :: DomainMetadataResponse)

-- | The response's http status code.
domainMetadataResponse_httpStatus :: Lens.Lens' DomainMetadataResponse Prelude.Int
domainMetadataResponse_httpStatus = Lens.lens (\DomainMetadataResponse' {httpStatus} -> httpStatus) (\s@DomainMetadataResponse' {} a -> s {httpStatus = a} :: DomainMetadataResponse)

instance Prelude.NFData DomainMetadataResponse where
  rnf DomainMetadataResponse' {..} =
    Prelude.rnf attributeNamesSizeBytes
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf attributeNameCount
      `Prelude.seq` Prelude.rnf attributeValueCount
      `Prelude.seq` Prelude.rnf itemNamesSizeBytes
      `Prelude.seq` Prelude.rnf attributeValuesSizeBytes
      `Prelude.seq` Prelude.rnf httpStatus
