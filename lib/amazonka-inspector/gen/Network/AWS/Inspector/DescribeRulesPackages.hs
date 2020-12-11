{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeRulesPackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the rules packages that are specified by the ARNs of the rules packages.
module Network.AWS.Inspector.DescribeRulesPackages
  ( -- * Creating a request
    DescribeRulesPackages (..),
    mkDescribeRulesPackages,

    -- ** Request lenses
    drpLocale,
    drpRulesPackageARNs,

    -- * Destructuring the response
    DescribeRulesPackagesResponse (..),
    mkDescribeRulesPackagesResponse,

    -- ** Response lenses
    drprsResponseStatus,
    drprsRulesPackages,
    drprsFailedItems,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRulesPackages' smart constructor.
data DescribeRulesPackages = DescribeRulesPackages'
  { locale ::
      Lude.Maybe Locale,
    rulesPackageARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRulesPackages' with the minimum fields required to make a request.
--
-- * 'locale' - The locale that you want to translate a rules package description into.
-- * 'rulesPackageARNs' - The ARN that specifies the rules package that you want to describe.
mkDescribeRulesPackages ::
  -- | 'rulesPackageARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeRulesPackages
mkDescribeRulesPackages pRulesPackageARNs_ =
  DescribeRulesPackages'
    { locale = Lude.Nothing,
      rulesPackageARNs = pRulesPackageARNs_
    }

-- | The locale that you want to translate a rules package description into.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLocale :: Lens.Lens' DescribeRulesPackages (Lude.Maybe Locale)
drpLocale = Lens.lens (locale :: DescribeRulesPackages -> Lude.Maybe Locale) (\s a -> s {locale = a} :: DescribeRulesPackages)
{-# DEPRECATED drpLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The ARN that specifies the rules package that you want to describe.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRulesPackageARNs :: Lens.Lens' DescribeRulesPackages (Lude.NonEmpty Lude.Text)
drpRulesPackageARNs = Lens.lens (rulesPackageARNs :: DescribeRulesPackages -> Lude.NonEmpty Lude.Text) (\s a -> s {rulesPackageARNs = a} :: DescribeRulesPackages)
{-# DEPRECATED drpRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

instance Lude.AWSRequest DescribeRulesPackages where
  type Rs DescribeRulesPackages = DescribeRulesPackagesResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRulesPackagesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "rulesPackages" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeRulesPackages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DescribeRulesPackages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRulesPackages where
  toJSON DescribeRulesPackages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            Lude.Just ("rulesPackageArns" Lude..= rulesPackageARNs)
          ]
      )

instance Lude.ToPath DescribeRulesPackages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRulesPackages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRulesPackagesResponse' smart constructor.
data DescribeRulesPackagesResponse = DescribeRulesPackagesResponse'
  { responseStatus ::
      Lude.Int,
    rulesPackages :: [RulesPackage],
    failedItems ::
      Lude.HashMap
        Lude.Text
        (FailedItemDetails)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRulesPackagesResponse' with the minimum fields required to make a request.
--
-- * 'failedItems' - Rules package details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
-- * 'rulesPackages' - Information about the rules package.
mkDescribeRulesPackagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRulesPackagesResponse
mkDescribeRulesPackagesResponse pResponseStatus_ =
  DescribeRulesPackagesResponse'
    { responseStatus = pResponseStatus_,
      rulesPackages = Lude.mempty,
      failedItems = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DescribeRulesPackagesResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DescribeRulesPackagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRulesPackagesResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the rules package.
--
-- /Note:/ Consider using 'rulesPackages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsRulesPackages :: Lens.Lens' DescribeRulesPackagesResponse [RulesPackage]
drprsRulesPackages = Lens.lens (rulesPackages :: DescribeRulesPackagesResponse -> [RulesPackage]) (\s a -> s {rulesPackages = a} :: DescribeRulesPackagesResponse)
{-# DEPRECATED drprsRulesPackages "Use generic-lens or generic-optics with 'rulesPackages' instead." #-}

-- | Rules package details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsFailedItems :: Lens.Lens' DescribeRulesPackagesResponse (Lude.HashMap Lude.Text (FailedItemDetails))
drprsFailedItems = Lens.lens (failedItems :: DescribeRulesPackagesResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeRulesPackagesResponse)
{-# DEPRECATED drprsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}
