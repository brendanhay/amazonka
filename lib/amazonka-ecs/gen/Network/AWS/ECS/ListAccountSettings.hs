{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListAccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account settings for a specified principal.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListAccountSettings
  ( -- * Creating a request
    ListAccountSettings (..),
    mkListAccountSettings,

    -- ** Request lenses
    lasValue,
    lasNextToken,
    lasName,
    lasPrincipalARN,
    lasEffectiveSettings,
    lasMaxResults,

    -- * Destructuring the response
    ListAccountSettingsResponse (..),
    mkListAccountSettingsResponse,

    -- ** Response lenses
    lasrsSettings,
    lasrsNextToken,
    lasrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAccountSettings' smart constructor.
data ListAccountSettings = ListAccountSettings'
  { -- | The value of the account settings with which to filter results. You must also specify an account setting name to use this parameter.
    value :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a @ListAccountSettings@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the account setting you want to list the settings for.
    name :: Lude.Maybe SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the account settings are listed only for the authenticated user.
    principalARN :: Lude.Maybe Lude.Text,
    -- | Specifies whether to return the effective settings. If @true@ , the account settings for the root user or the default setting for the @principalArn@ are returned. If @false@ , the account settings for the @principalArn@ are returned if they are set. Otherwise, no account settings are returned.
    effectiveSettings :: Lude.Maybe Lude.Bool,
    -- | The maximum number of account setting results returned by @ListAccountSettings@ in paginated output. When this parameter is used, @ListAccountSettings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAccountSettings@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @ListAccountSettings@ returns up to 10 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccountSettings' with the minimum fields required to make a request.
--
-- * 'value' - The value of the account settings with which to filter results. You must also specify an account setting name to use this parameter.
-- * 'nextToken' - The @nextToken@ value returned from a @ListAccountSettings@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'name' - The name of the account setting you want to list the settings for.
-- * 'principalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the account settings are listed only for the authenticated user.
-- * 'effectiveSettings' - Specifies whether to return the effective settings. If @true@ , the account settings for the root user or the default setting for the @principalArn@ are returned. If @false@ , the account settings for the @principalArn@ are returned if they are set. Otherwise, no account settings are returned.
-- * 'maxResults' - The maximum number of account setting results returned by @ListAccountSettings@ in paginated output. When this parameter is used, @ListAccountSettings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAccountSettings@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @ListAccountSettings@ returns up to 10 results and a @nextToken@ value if applicable.
mkListAccountSettings ::
  ListAccountSettings
mkListAccountSettings =
  ListAccountSettings'
    { value = Lude.Nothing,
      nextToken = Lude.Nothing,
      name = Lude.Nothing,
      principalARN = Lude.Nothing,
      effectiveSettings = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The value of the account settings with which to filter results. You must also specify an account setting name to use this parameter.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasValue :: Lens.Lens' ListAccountSettings (Lude.Maybe Lude.Text)
lasValue = Lens.lens (value :: ListAccountSettings -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ListAccountSettings)
{-# DEPRECATED lasValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The @nextToken@ value returned from a @ListAccountSettings@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListAccountSettings (Lude.Maybe Lude.Text)
lasNextToken = Lens.lens (nextToken :: ListAccountSettings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAccountSettings)
{-# DEPRECATED lasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the account setting you want to list the settings for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasName :: Lens.Lens' ListAccountSettings (Lude.Maybe SettingName)
lasName = Lens.lens (name :: ListAccountSettings -> Lude.Maybe SettingName) (\s a -> s {name = a} :: ListAccountSettings)
{-# DEPRECATED lasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the account settings are listed only for the authenticated user.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasPrincipalARN :: Lens.Lens' ListAccountSettings (Lude.Maybe Lude.Text)
lasPrincipalARN = Lens.lens (principalARN :: ListAccountSettings -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: ListAccountSettings)
{-# DEPRECATED lasPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | Specifies whether to return the effective settings. If @true@ , the account settings for the root user or the default setting for the @principalArn@ are returned. If @false@ , the account settings for the @principalArn@ are returned if they are set. Otherwise, no account settings are returned.
--
-- /Note:/ Consider using 'effectiveSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasEffectiveSettings :: Lens.Lens' ListAccountSettings (Lude.Maybe Lude.Bool)
lasEffectiveSettings = Lens.lens (effectiveSettings :: ListAccountSettings -> Lude.Maybe Lude.Bool) (\s a -> s {effectiveSettings = a} :: ListAccountSettings)
{-# DEPRECATED lasEffectiveSettings "Use generic-lens or generic-optics with 'effectiveSettings' instead." #-}

-- | The maximum number of account setting results returned by @ListAccountSettings@ in paginated output. When this parameter is used, @ListAccountSettings@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAccountSettings@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @ListAccountSettings@ returns up to 10 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasMaxResults :: Lens.Lens' ListAccountSettings (Lude.Maybe Lude.Int)
lasMaxResults = Lens.lens (maxResults :: ListAccountSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListAccountSettings)
{-# DEPRECATED lasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAccountSettings where
  page rq rs
    | Page.stop (rs Lens.^. lasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasrsSettings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lasNextToken Lens..~ rs Lens.^. lasrsNextToken

instance Lude.AWSRequest ListAccountSettings where
  type Rs ListAccountSettings = ListAccountSettingsResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAccountSettingsResponse'
            Lude.<$> (x Lude..?> "settings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAccountSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListAccountSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAccountSettings where
  toJSON ListAccountSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("value" Lude..=) Lude.<$> value,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("name" Lude..=) Lude.<$> name,
            ("principalArn" Lude..=) Lude.<$> principalARN,
            ("effectiveSettings" Lude..=) Lude.<$> effectiveSettings,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAccountSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAccountSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAccountSettingsResponse' smart constructor.
data ListAccountSettingsResponse = ListAccountSettingsResponse'
  { -- | The account settings for the resource.
    settings :: Lude.Maybe [Setting],
    -- | The @nextToken@ value to include in a future @ListAccountSettings@ request. When the results of a @ListAccountSettings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccountSettingsResponse' with the minimum fields required to make a request.
--
-- * 'settings' - The account settings for the resource.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListAccountSettings@ request. When the results of a @ListAccountSettings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListAccountSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAccountSettingsResponse
mkListAccountSettingsResponse pResponseStatus_ =
  ListAccountSettingsResponse'
    { settings = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The account settings for the resource.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsSettings :: Lens.Lens' ListAccountSettingsResponse (Lude.Maybe [Setting])
lasrsSettings = Lens.lens (settings :: ListAccountSettingsResponse -> Lude.Maybe [Setting]) (\s a -> s {settings = a} :: ListAccountSettingsResponse)
{-# DEPRECATED lasrsSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The @nextToken@ value to include in a future @ListAccountSettings@ request. When the results of a @ListAccountSettings@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsNextToken :: Lens.Lens' ListAccountSettingsResponse (Lude.Maybe Lude.Text)
lasrsNextToken = Lens.lens (nextToken :: ListAccountSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAccountSettingsResponse)
{-# DEPRECATED lasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsResponseStatus :: Lens.Lens' ListAccountSettingsResponse Lude.Int
lasrsResponseStatus = Lens.lens (responseStatus :: ListAccountSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAccountSettingsResponse)
{-# DEPRECATED lasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
