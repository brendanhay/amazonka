{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DomainMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the domain, including when the domain was created, the number of items and attributes in the domain, and the size of the attribute names and values.
module Network.AWS.SDB.DomainMetadata
  ( -- * Creating a request
    DomainMetadata (..),
    mkDomainMetadata,

    -- ** Request lenses
    dmDomainName,

    -- * Destructuring the response
    DomainMetadataResponse (..),
    mkDomainMetadataResponse,

    -- ** Response lenses
    dmrsItemNamesSizeBytes,
    dmrsAttributeValuesSizeBytes,
    dmrsAttributeNameCount,
    dmrsAttributeNamesSizeBytes,
    dmrsAttributeValueCount,
    dmrsItemCount,
    dmrsTimestamp,
    dmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkDomainMetadata' smart constructor.
newtype DomainMetadata = DomainMetadata' {domainName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainMetadata' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain for which to display the metadata of.
mkDomainMetadata ::
  -- | 'domainName'
  Lude.Text ->
  DomainMetadata
mkDomainMetadata pDomainName_ =
  DomainMetadata' {domainName = pDomainName_}

-- | The name of the domain for which to display the metadata of.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDomainName :: Lens.Lens' DomainMetadata Lude.Text
dmDomainName = Lens.lens (domainName :: DomainMetadata -> Lude.Text) (\s a -> s {domainName = a} :: DomainMetadata)
{-# DEPRECATED dmDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DomainMetadata where
  type Rs DomainMetadata = DomainMetadataResponse
  request = Req.postQuery sdbService
  response =
    Res.receiveXMLWrapper
      "DomainMetadataResult"
      ( \s h x ->
          DomainMetadataResponse'
            Lude.<$> (x Lude..@? "ItemNamesSizeBytes")
            Lude.<*> (x Lude..@? "AttributeValuesSizeBytes")
            Lude.<*> (x Lude..@? "AttributeNameCount")
            Lude.<*> (x Lude..@? "AttributeNamesSizeBytes")
            Lude.<*> (x Lude..@? "AttributeValueCount")
            Lude.<*> (x Lude..@? "ItemCount")
            Lude.<*> (x Lude..@? "Timestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DomainMetadata where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DomainMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery DomainMetadata where
  toQuery DomainMetadata' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DomainMetadata" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | /See:/ 'mkDomainMetadataResponse' smart constructor.
data DomainMetadataResponse = DomainMetadataResponse'
  { itemNamesSizeBytes ::
      Lude.Maybe Lude.Integer,
    attributeValuesSizeBytes ::
      Lude.Maybe Lude.Integer,
    attributeNameCount :: Lude.Maybe Lude.Int,
    attributeNamesSizeBytes ::
      Lude.Maybe Lude.Integer,
    attributeValueCount :: Lude.Maybe Lude.Int,
    itemCount :: Lude.Maybe Lude.Int,
    timestamp :: Lude.Maybe Lude.Int,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainMetadataResponse' with the minimum fields required to make a request.
--
-- * 'attributeNameCount' - The number of unique attribute names in the domain.
-- * 'attributeNamesSizeBytes' - The total size of all unique attribute names in the domain, in bytes.
-- * 'attributeValueCount' - The number of all attribute name/value pairs in the domain.
-- * 'attributeValuesSizeBytes' - The total size of all attribute values in the domain, in bytes.
-- * 'itemCount' - The number of all items in the domain.
-- * 'itemNamesSizeBytes' - The total size of all item names in the domain, in bytes.
-- * 'responseStatus' - The response status code.
-- * 'timestamp' - The data and time when metadata was calculated, in Epoch (UNIX) seconds.
mkDomainMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DomainMetadataResponse
mkDomainMetadataResponse pResponseStatus_ =
  DomainMetadataResponse'
    { itemNamesSizeBytes = Lude.Nothing,
      attributeValuesSizeBytes = Lude.Nothing,
      attributeNameCount = Lude.Nothing,
      attributeNamesSizeBytes = Lude.Nothing,
      attributeValueCount = Lude.Nothing,
      itemCount = Lude.Nothing,
      timestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The total size of all item names in the domain, in bytes.
--
-- /Note:/ Consider using 'itemNamesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsItemNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Integer)
dmrsItemNamesSizeBytes = Lens.lens (itemNamesSizeBytes :: DomainMetadataResponse -> Lude.Maybe Lude.Integer) (\s a -> s {itemNamesSizeBytes = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsItemNamesSizeBytes "Use generic-lens or generic-optics with 'itemNamesSizeBytes' instead." #-}

-- | The total size of all attribute values in the domain, in bytes.
--
-- /Note:/ Consider using 'attributeValuesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsAttributeValuesSizeBytes :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Integer)
dmrsAttributeValuesSizeBytes = Lens.lens (attributeValuesSizeBytes :: DomainMetadataResponse -> Lude.Maybe Lude.Integer) (\s a -> s {attributeValuesSizeBytes = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsAttributeValuesSizeBytes "Use generic-lens or generic-optics with 'attributeValuesSizeBytes' instead." #-}

-- | The number of unique attribute names in the domain.
--
-- /Note:/ Consider using 'attributeNameCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsAttributeNameCount :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Int)
dmrsAttributeNameCount = Lens.lens (attributeNameCount :: DomainMetadataResponse -> Lude.Maybe Lude.Int) (\s a -> s {attributeNameCount = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsAttributeNameCount "Use generic-lens or generic-optics with 'attributeNameCount' instead." #-}

-- | The total size of all unique attribute names in the domain, in bytes.
--
-- /Note:/ Consider using 'attributeNamesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsAttributeNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Integer)
dmrsAttributeNamesSizeBytes = Lens.lens (attributeNamesSizeBytes :: DomainMetadataResponse -> Lude.Maybe Lude.Integer) (\s a -> s {attributeNamesSizeBytes = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsAttributeNamesSizeBytes "Use generic-lens or generic-optics with 'attributeNamesSizeBytes' instead." #-}

-- | The number of all attribute name/value pairs in the domain.
--
-- /Note:/ Consider using 'attributeValueCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsAttributeValueCount :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Int)
dmrsAttributeValueCount = Lens.lens (attributeValueCount :: DomainMetadataResponse -> Lude.Maybe Lude.Int) (\s a -> s {attributeValueCount = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsAttributeValueCount "Use generic-lens or generic-optics with 'attributeValueCount' instead." #-}

-- | The number of all items in the domain.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsItemCount :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Int)
dmrsItemCount = Lens.lens (itemCount :: DomainMetadataResponse -> Lude.Maybe Lude.Int) (\s a -> s {itemCount = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsTimestamp :: Lens.Lens' DomainMetadataResponse (Lude.Maybe Lude.Int)
dmrsTimestamp = Lens.lens (timestamp :: DomainMetadataResponse -> Lude.Maybe Lude.Int) (\s a -> s {timestamp = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsResponseStatus :: Lens.Lens' DomainMetadataResponse Lude.Int
dmrsResponseStatus = Lens.lens (responseStatus :: DomainMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DomainMetadataResponse)
{-# DEPRECATED dmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
