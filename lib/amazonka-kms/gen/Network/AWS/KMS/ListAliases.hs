{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of aliases in the caller's AWS account and region. You cannot list aliases in other accounts. For more information about aliases, see 'CreateAlias' .
--
-- By default, the ListAliases command returns all aliases in the account and region. To get only the aliases that point to a particular customer master key (CMK), use the @KeyId@ parameter.
-- The @ListAliases@ response can include aliases that you created and associated with your customer managed CMKs, and aliases that AWS created and associated with AWS managed CMKs in your account. You can recognize AWS aliases because their names have the format @aws/<service-name>@ , such as @aws/dynamodb@ .
-- The response might also include aliases that have no @TargetKeyId@ field. These are predefined aliases that AWS has created but has not yet associated with a CMK. Aliases that AWS creates in your account, including predefined aliases, do not count against your <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#aliases-limit AWS KMS aliases quota> .
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListAliases
  ( -- * Creating a request
    ListAliases (..),
    mkListAliases,

    -- ** Request lenses
    laKeyId,
    laMarker,
    laLimit,

    -- * Destructuring the response
    ListAliasesResponse (..),
    mkListAliasesResponse,

    -- ** Response lenses
    larsTruncated,
    larsAliases,
    larsNextMarker,
    larsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { keyId :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- * 'keyId' - Lists only aliases that refer to the specified CMK. The value of this parameter can be the ID or Amazon Resource Name (ARN) of a CMK in the caller's account and region. You cannot use an alias name or alias ARN in this value.
--
-- This parameter is optional. If you omit it, @ListAliases@ returns all aliases in the account and region.
-- * 'limit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
mkListAliases ::
  ListAliases
mkListAliases =
  ListAliases'
    { keyId = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Lists only aliases that refer to the specified CMK. The value of this parameter can be the ID or Amazon Resource Name (ARN) of a CMK in the caller's account and region. You cannot use an alias name or alias ARN in this value.
--
-- This parameter is optional. If you omit it, @ListAliases@ returns all aliases in the account and region.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laKeyId :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laKeyId = Lens.lens (keyId :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: ListAliases)
{-# DEPRECATED laKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMarker :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laMarker = Lens.lens (marker :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAliases)
{-# DEPRECATED laMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListAliases (Lude.Maybe Lude.Natural)
laLimit = Lens.lens (limit :: ListAliases -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListAliases)
{-# DEPRECATED laLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListAliases where
  page rq rs
    | Page.stop (rs Lens.^. larsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. larsNextMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laMarker Lens..~ rs Lens.^. larsNextMarker

instance Lude.AWSRequest ListAliases where
  type Rs ListAliases = ListAliasesResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Lude.<$> (x Lude..?> "Truncated")
            Lude.<*> (x Lude..?> "Aliases" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAliases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ListAliases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyId" Lude..=) Lude.<$> keyId,
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListAliases where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAliases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { truncated ::
      Lude.Maybe Lude.Bool,
    aliases :: Lude.Maybe [AliasListEntry],
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- * 'aliases' - A list of aliases.
-- * 'nextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
-- * 'responseStatus' - The response status code.
-- * 'truncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
mkListAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAliasesResponse
mkListAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { truncated = Lude.Nothing,
      aliases = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsTruncated :: Lens.Lens' ListAliasesResponse (Lude.Maybe Lude.Bool)
larsTruncated = Lens.lens (truncated :: ListAliasesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: ListAliasesResponse)
{-# DEPRECATED larsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | A list of aliases.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAliases :: Lens.Lens' ListAliasesResponse (Lude.Maybe [AliasListEntry])
larsAliases = Lens.lens (aliases :: ListAliasesResponse -> Lude.Maybe [AliasListEntry]) (\s a -> s {aliases = a} :: ListAliasesResponse)
{-# DEPRECATED larsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextMarker :: Lens.Lens' ListAliasesResponse (Lude.Maybe Lude.Text)
larsNextMarker = Lens.lens (nextMarker :: ListAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListAliasesResponse)
{-# DEPRECATED larsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAliasesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAliasesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
