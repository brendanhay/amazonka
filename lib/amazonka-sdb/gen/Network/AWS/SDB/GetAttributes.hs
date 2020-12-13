{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gaItemName,
    gaConsistentRead,
    gaDomainName,
    gaAttributeNames,

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
  { -- | The name of the item.
    itemName :: Lude.Text,
    -- | @true@
    consistentRead :: Lude.Maybe Lude.Bool,
    -- | The name of the domain in which to perform the operation.
    domainName :: Lude.Text,
    -- | The names of the attributes.
    attributeNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAttributes' with the minimum fields required to make a request.
--
-- * 'itemName' - The name of the item.
-- * 'consistentRead' - @true@
-- * 'domainName' - The name of the domain in which to perform the operation.
-- * 'attributeNames' - The names of the attributes.
mkGetAttributes ::
  -- | 'itemName'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  GetAttributes
mkGetAttributes pItemName_ pDomainName_ =
  GetAttributes'
    { itemName = pItemName_,
      consistentRead = Lude.Nothing,
      domainName = pDomainName_,
      attributeNames = Lude.Nothing
    }

-- | The name of the item.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaItemName :: Lens.Lens' GetAttributes Lude.Text
gaItemName = Lens.lens (itemName :: GetAttributes -> Lude.Text) (\s a -> s {itemName = a} :: GetAttributes)
{-# DEPRECATED gaItemName "Use generic-lens or generic-optics with 'itemName' instead." #-}

-- | @true@
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaConsistentRead :: Lens.Lens' GetAttributes (Lude.Maybe Lude.Bool)
gaConsistentRead = Lens.lens (consistentRead :: GetAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: GetAttributes)
{-# DEPRECATED gaConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDomainName :: Lens.Lens' GetAttributes Lude.Text
gaDomainName = Lens.lens (domainName :: GetAttributes -> Lude.Text) (\s a -> s {domainName = a} :: GetAttributes)
{-# DEPRECATED gaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The names of the attributes.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAttributeNames :: Lens.Lens' GetAttributes (Lude.Maybe [Lude.Text])
gaAttributeNames = Lens.lens (attributeNames :: GetAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {attributeNames = a} :: GetAttributes)
{-# DEPRECATED gaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

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
        "ItemName" Lude.=: itemName,
        "ConsistentRead" Lude.=: consistentRead,
        "DomainName" Lude.=: domainName,
        Lude.toQuery
          (Lude.toQueryList "AttributeName" Lude.<$> attributeNames)
      ]

-- | /See:/ 'mkGetAttributesResponse' smart constructor.
data GetAttributesResponse = GetAttributesResponse'
  { -- | The list of attributes returned by the operation.
    attributes :: Lude.Maybe [Attribute],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
