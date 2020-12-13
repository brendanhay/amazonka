{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetAPIKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ApiKeys' resource.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetAPIKeys
  ( -- * Creating a request
    GetAPIKeys (..),
    mkGetAPIKeys,

    -- ** Request lenses
    gakIncludeValues,
    gakCustomerId,
    gakNameQuery,
    gakLimit,
    gakPosition,

    -- * Destructuring the response
    GetAPIKeysResponse (..),
    mkGetAPIKeysResponse,

    -- ** Response lenses
    gakrsItems,
    gakrsWarnings,
    gakrsPosition,
    gakrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to get information about the current 'ApiKeys' resource.
--
-- /See:/ 'mkGetAPIKeys' smart constructor.
data GetAPIKeys = GetAPIKeys'
  { -- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains key values.
    includeValues :: Lude.Maybe Lude.Bool,
    -- | The identifier of a customer in AWS Marketplace or an external system, such as a developer portal.
    customerId :: Lude.Maybe Lude.Text,
    -- | The name of queried API keys.
    nameQuery :: Lude.Maybe Lude.Text,
    -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPIKeys' with the minimum fields required to make a request.
--
-- * 'includeValues' - A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains key values.
-- * 'customerId' - The identifier of a customer in AWS Marketplace or an external system, such as a developer portal.
-- * 'nameQuery' - The name of queried API keys.
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetAPIKeys ::
  GetAPIKeys
mkGetAPIKeys =
  GetAPIKeys'
    { includeValues = Lude.Nothing,
      customerId = Lude.Nothing,
      nameQuery = Lude.Nothing,
      limit = Lude.Nothing,
      position = Lude.Nothing
    }

-- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains key values.
--
-- /Note:/ Consider using 'includeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakIncludeValues :: Lens.Lens' GetAPIKeys (Lude.Maybe Lude.Bool)
gakIncludeValues = Lens.lens (includeValues :: GetAPIKeys -> Lude.Maybe Lude.Bool) (\s a -> s {includeValues = a} :: GetAPIKeys)
{-# DEPRECATED gakIncludeValues "Use generic-lens or generic-optics with 'includeValues' instead." #-}

-- | The identifier of a customer in AWS Marketplace or an external system, such as a developer portal.
--
-- /Note:/ Consider using 'customerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakCustomerId :: Lens.Lens' GetAPIKeys (Lude.Maybe Lude.Text)
gakCustomerId = Lens.lens (customerId :: GetAPIKeys -> Lude.Maybe Lude.Text) (\s a -> s {customerId = a} :: GetAPIKeys)
{-# DEPRECATED gakCustomerId "Use generic-lens or generic-optics with 'customerId' instead." #-}

-- | The name of queried API keys.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakNameQuery :: Lens.Lens' GetAPIKeys (Lude.Maybe Lude.Text)
gakNameQuery = Lens.lens (nameQuery :: GetAPIKeys -> Lude.Maybe Lude.Text) (\s a -> s {nameQuery = a} :: GetAPIKeys)
{-# DEPRECATED gakNameQuery "Use generic-lens or generic-optics with 'nameQuery' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakLimit :: Lens.Lens' GetAPIKeys (Lude.Maybe Lude.Int)
gakLimit = Lens.lens (limit :: GetAPIKeys -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetAPIKeys)
{-# DEPRECATED gakLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakPosition :: Lens.Lens' GetAPIKeys (Lude.Maybe Lude.Text)
gakPosition = Lens.lens (position :: GetAPIKeys -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetAPIKeys)
{-# DEPRECATED gakPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetAPIKeys where
  page rq rs
    | Page.stop (rs Lens.^. gakrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gakrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gakPosition Lens..~ rs Lens.^. gakrsPosition

instance Lude.AWSRequest GetAPIKeys where
  type Rs GetAPIKeys = GetAPIKeysResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAPIKeysResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "warnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAPIKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetAPIKeys where
  toPath = Lude.const "/apikeys"

instance Lude.ToQuery GetAPIKeys where
  toQuery GetAPIKeys' {..} =
    Lude.mconcat
      [ "includeValues" Lude.=: includeValues,
        "customerId" Lude.=: customerId,
        "name" Lude.=: nameQuery,
        "limit" Lude.=: limit,
        "position" Lude.=: position
      ]

-- | Represents a collection of API keys as represented by an 'ApiKeys' resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys>
--
-- /See:/ 'mkGetAPIKeysResponse' smart constructor.
data GetAPIKeysResponse = GetAPIKeysResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [APIKey],
    -- | A list of warning messages logged during the import of API keys when the @failOnWarnings@ option is set to true.
    warnings :: Lude.Maybe [Lude.Text],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPIKeysResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'warnings' - A list of warning messages logged during the import of API keys when the @failOnWarnings@ option is set to true.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetAPIKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAPIKeysResponse
mkGetAPIKeysResponse pResponseStatus_ =
  GetAPIKeysResponse'
    { items = Lude.Nothing,
      warnings = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrsItems :: Lens.Lens' GetAPIKeysResponse (Lude.Maybe [APIKey])
gakrsItems = Lens.lens (items :: GetAPIKeysResponse -> Lude.Maybe [APIKey]) (\s a -> s {items = a} :: GetAPIKeysResponse)
{-# DEPRECATED gakrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | A list of warning messages logged during the import of API keys when the @failOnWarnings@ option is set to true.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrsWarnings :: Lens.Lens' GetAPIKeysResponse (Lude.Maybe [Lude.Text])
gakrsWarnings = Lens.lens (warnings :: GetAPIKeysResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {warnings = a} :: GetAPIKeysResponse)
{-# DEPRECATED gakrsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrsPosition :: Lens.Lens' GetAPIKeysResponse (Lude.Maybe Lude.Text)
gakrsPosition = Lens.lens (position :: GetAPIKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetAPIKeysResponse)
{-# DEPRECATED gakrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakrsResponseStatus :: Lens.Lens' GetAPIKeysResponse Lude.Int
gakrsResponseStatus = Lens.lens (responseStatus :: GetAPIKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAPIKeysResponse)
{-# DEPRECATED gakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
