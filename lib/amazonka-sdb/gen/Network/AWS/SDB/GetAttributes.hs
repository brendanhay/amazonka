{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.GetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the attributes associated with the specified item. Optionally, the attributes returned can be limited to one or more attributes by specifying an attribute name parameter.
--
-- If the item does not exist on the replica that was accessed for this operation, an empty set is returned. The system does not return an error as it cannot guarantee the item does not exist on other replicas.
module Network.AWS.SDB.GetAttributes
  ( -- * Creating a request
    GetAttributes (..),
    mkGetAttributes,

    -- ** Request lenses
    gaConsistentRead,
    gaAttributeNames,
    gaDomainName,
    gaItemName,

    -- * Destructuring the response
    GetAttributesResponse (..),
    mkGetAttributesResponse,

    -- ** Response lenses
    garsAttributes,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkGetAttributes' smart constructor.
data GetAttributes = GetAttributes'
  { consistentRead ::
      Lude.Maybe Lude.Bool,
    attributeNames :: Lude.Maybe [Lude.Text],
    domainName :: Lude.Text,
    itemName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - The names of the attributes.
-- * 'consistentRead' - @true@
-- * 'domainName' - The name of the domain in which to perform the operation.
-- * 'itemName' - The name of the item.
mkGetAttributes ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'itemName'
  Lude.Text ->
  GetAttributes
mkGetAttributes pDomainName_ pItemName_ =
  GetAttributes'
    { consistentRead = Lude.Nothing,
      attributeNames = Lude.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_
    }

-- | @true@
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaConsistentRead :: Lens.Lens' GetAttributes (Lude.Maybe Lude.Bool)
gaConsistentRead = Lens.lens (consistentRead :: GetAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: GetAttributes)
{-# DEPRECATED gaConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | The names of the attributes.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAttributeNames :: Lens.Lens' GetAttributes (Lude.Maybe [Lude.Text])
gaAttributeNames = Lens.lens (attributeNames :: GetAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {attributeNames = a} :: GetAttributes)
{-# DEPRECATED gaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDomainName :: Lens.Lens' GetAttributes Lude.Text
gaDomainName = Lens.lens (domainName :: GetAttributes -> Lude.Text) (\s a -> s {domainName = a} :: GetAttributes)
{-# DEPRECATED gaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaItemName :: Lens.Lens' GetAttributes Lude.Text
gaItemName = Lens.lens (itemName :: GetAttributes -> Lude.Text) (\s a -> s {itemName = a} :: GetAttributes)
{-# DEPRECATED gaItemName "Use generic-lens or generic-optics with 'itemName' instead." #-}

instance Lude.AWSRequest GetAttributes where
  type Rs GetAttributes = GetAttributesResponse
  request = Req.postQuery sdbService
  response =
    Res.receiveXMLWrapper
      "GetAttributesResult"
      ( \s h x ->
          GetAttributesResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "Attribute") x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAttributes where
  toQuery GetAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "ConsistentRead" Lude.=: consistentRead,
        Lude.toQuery
          (Lude.toQueryList "AttributeName" Lude.<$> attributeNames),
        "DomainName" Lude.=: domainName,
        "ItemName" Lude.=: itemName
      ]

-- | /See:/ 'mkGetAttributesResponse' smart constructor.
data GetAttributesResponse = GetAttributesResponse'
  { attributes ::
      Lude.Maybe [Attribute],
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

-- | Creates a value of 'GetAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The list of attributes returned by the operation.
-- * 'responseStatus' - The response status code.
mkGetAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAttributesResponse
mkGetAttributesResponse pResponseStatus_ =
  GetAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of attributes returned by the operation.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsAttributes :: Lens.Lens' GetAttributesResponse (Lude.Maybe [Attribute])
garsAttributes = Lens.lens (attributes :: GetAttributesResponse -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: GetAttributesResponse)
{-# DEPRECATED garsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAttributesResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAttributesResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
