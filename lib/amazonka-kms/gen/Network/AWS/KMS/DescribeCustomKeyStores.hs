{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DescribeCustomKeyStores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores> in the account and region.
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
-- By default, this operation returns information about all custom key stores in the account and region. To get only information about a particular custom key store, use either the @CustomKeyStoreName@ or @CustomKeyStoreId@ parameter (but not both).
-- To determine whether the custom key store is connected to its AWS CloudHSM cluster, use the @ConnectionState@ element in the response. If an attempt to connect the custom key store failed, the @ConnectionState@ value is @FAILED@ and the @ConnectionErrorCode@ element in the response indicates the cause of the failure. For help interpreting the @ConnectionErrorCode@ , see 'CustomKeyStoresListEntry' .
-- Custom key stores have a @DISCONNECTED@ connection state if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If your custom key store state is @CONNECTED@ but you are having trouble using it, make sure that its associated AWS CloudHSM cluster is active and contains the minimum number of HSMs required for the operation, if any.
-- For help repairing your custom key store, see the <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting Custom Key Stores> topic in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DescribeCustomKeyStores
  ( -- * Creating a request
    DescribeCustomKeyStores (..),
    mkDescribeCustomKeyStores,

    -- ** Request lenses
    dCustomKeyStoreName,
    dMarker,
    dLimit,
    dCustomKeyStoreId,

    -- * Destructuring the response
    DescribeCustomKeyStoresResponse (..),
    mkDescribeCustomKeyStoresResponse,

    -- ** Response lenses
    dckssrsTruncated,
    dckssrsNextMarker,
    dckssrsCustomKeyStores,
    dckssrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCustomKeyStores' smart constructor.
data DescribeCustomKeyStores = DescribeCustomKeyStores'
  { -- | Gets only information about the specified custom key store. Enter the friendly name of the custom key store.
    --
    -- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
    customKeyStoreName :: Lude.Maybe Lude.Text,
    -- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
    limit :: Lude.Maybe Lude.Natural,
    -- | Gets only information about the specified custom key store. Enter the key store ID.
    --
    -- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
    customKeyStoreId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCustomKeyStores' with the minimum fields required to make a request.
--
-- * 'customKeyStoreName' - Gets only information about the specified custom key store. Enter the friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
-- * 'limit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
-- * 'customKeyStoreId' - Gets only information about the specified custom key store. Enter the key store ID.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
mkDescribeCustomKeyStores ::
  DescribeCustomKeyStores
mkDescribeCustomKeyStores =
  DescribeCustomKeyStores'
    { customKeyStoreName = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      customKeyStoreId = Lude.Nothing
    }

-- | Gets only information about the specified custom key store. Enter the friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
--
-- /Note:/ Consider using 'customKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCustomKeyStoreName :: Lens.Lens' DescribeCustomKeyStores (Lude.Maybe Lude.Text)
dCustomKeyStoreName = Lens.lens (customKeyStoreName :: DescribeCustomKeyStores -> Lude.Maybe Lude.Text) (\s a -> s {customKeyStoreName = a} :: DescribeCustomKeyStores)
{-# DEPRECATED dCustomKeyStoreName "Use generic-lens or generic-optics with 'customKeyStoreName' instead." #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeCustomKeyStores (Lude.Maybe Lude.Text)
dMarker = Lens.lens (marker :: DescribeCustomKeyStores -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCustomKeyStores)
{-# DEPRECATED dMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLimit :: Lens.Lens' DescribeCustomKeyStores (Lude.Maybe Lude.Natural)
dLimit = Lens.lens (limit :: DescribeCustomKeyStores -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeCustomKeyStores)
{-# DEPRECATED dLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Gets only information about the specified custom key store. Enter the key store ID.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCustomKeyStoreId :: Lens.Lens' DescribeCustomKeyStores (Lude.Maybe Lude.Text)
dCustomKeyStoreId = Lens.lens (customKeyStoreId :: DescribeCustomKeyStores -> Lude.Maybe Lude.Text) (\s a -> s {customKeyStoreId = a} :: DescribeCustomKeyStores)
{-# DEPRECATED dCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Lude.AWSRequest DescribeCustomKeyStores where
  type Rs DescribeCustomKeyStores = DescribeCustomKeyStoresResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCustomKeyStoresResponse'
            Lude.<$> (x Lude..?> "Truncated")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "CustomKeyStores" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCustomKeyStores where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DescribeCustomKeyStores" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCustomKeyStores where
  toJSON DescribeCustomKeyStores' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomKeyStoreName" Lude..=) Lude.<$> customKeyStoreName,
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            ("CustomKeyStoreId" Lude..=) Lude.<$> customKeyStoreId
          ]
      )

instance Lude.ToPath DescribeCustomKeyStores where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCustomKeyStores where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCustomKeyStoresResponse' smart constructor.
data DescribeCustomKeyStoresResponse = DescribeCustomKeyStoresResponse'
  { -- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
    truncated :: Lude.Maybe Lude.Bool,
    -- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Contains metadata about each custom key store.
    customKeyStores :: Lude.Maybe [CustomKeyStoresListEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCustomKeyStoresResponse' with the minimum fields required to make a request.
--
-- * 'truncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
-- * 'nextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
-- * 'customKeyStores' - Contains metadata about each custom key store.
-- * 'responseStatus' - The response status code.
mkDescribeCustomKeyStoresResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCustomKeyStoresResponse
mkDescribeCustomKeyStoresResponse pResponseStatus_ =
  DescribeCustomKeyStoresResponse'
    { truncated = Lude.Nothing,
      nextMarker = Lude.Nothing,
      customKeyStores = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssrsTruncated :: Lens.Lens' DescribeCustomKeyStoresResponse (Lude.Maybe Lude.Bool)
dckssrsTruncated = Lens.lens (truncated :: DescribeCustomKeyStoresResponse -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: DescribeCustomKeyStoresResponse)
{-# DEPRECATED dckssrsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssrsNextMarker :: Lens.Lens' DescribeCustomKeyStoresResponse (Lude.Maybe Lude.Text)
dckssrsNextMarker = Lens.lens (nextMarker :: DescribeCustomKeyStoresResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeCustomKeyStoresResponse)
{-# DEPRECATED dckssrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Contains metadata about each custom key store.
--
-- /Note:/ Consider using 'customKeyStores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssrsCustomKeyStores :: Lens.Lens' DescribeCustomKeyStoresResponse (Lude.Maybe [CustomKeyStoresListEntry])
dckssrsCustomKeyStores = Lens.lens (customKeyStores :: DescribeCustomKeyStoresResponse -> Lude.Maybe [CustomKeyStoresListEntry]) (\s a -> s {customKeyStores = a} :: DescribeCustomKeyStoresResponse)
{-# DEPRECATED dckssrsCustomKeyStores "Use generic-lens or generic-optics with 'customKeyStores' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssrsResponseStatus :: Lens.Lens' DescribeCustomKeyStoresResponse Lude.Int
dckssrsResponseStatus = Lens.lens (responseStatus :: DescribeCustomKeyStoresResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCustomKeyStoresResponse)
{-# DEPRECATED dckssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
