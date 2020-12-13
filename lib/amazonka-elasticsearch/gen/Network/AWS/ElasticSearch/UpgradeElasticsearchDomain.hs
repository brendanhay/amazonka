{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your domain or perform an Upgrade eligibility check to a compatible Elasticsearch version.
module Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
  ( -- * Creating a request
    UpgradeElasticsearchDomain (..),
    mkUpgradeElasticsearchDomain,

    -- ** Request lenses
    uedDomainName,
    uedPerformCheckOnly,
    uedTargetVersion,

    -- * Destructuring the response
    UpgradeElasticsearchDomainResponse (..),
    mkUpgradeElasticsearchDomainResponse,

    -- ** Response lenses
    uedrsDomainName,
    uedrsPerformCheckOnly,
    uedrsTargetVersion,
    uedrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'UpgradeElasticsearchDomain' @ operation.
--
-- /See:/ 'mkUpgradeElasticsearchDomain' smart constructor.
data UpgradeElasticsearchDomain = UpgradeElasticsearchDomain'
  { domainName :: Lude.Text,
    -- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Lude.Maybe Lude.Bool,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeElasticsearchDomain' with the minimum fields required to make a request.
--
-- * 'domainName' -
-- * 'performCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
-- * 'targetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
mkUpgradeElasticsearchDomain ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'targetVersion'
  Lude.Text ->
  UpgradeElasticsearchDomain
mkUpgradeElasticsearchDomain pDomainName_ pTargetVersion_ =
  UpgradeElasticsearchDomain'
    { domainName = pDomainName_,
      performCheckOnly = Lude.Nothing,
      targetVersion = pTargetVersion_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedDomainName :: Lens.Lens' UpgradeElasticsearchDomain Lude.Text
uedDomainName = Lens.lens (domainName :: UpgradeElasticsearchDomain -> Lude.Text) (\s a -> s {domainName = a} :: UpgradeElasticsearchDomain)
{-# DEPRECATED uedDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
--
-- /Note:/ Consider using 'performCheckOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedPerformCheckOnly :: Lens.Lens' UpgradeElasticsearchDomain (Lude.Maybe Lude.Bool)
uedPerformCheckOnly = Lens.lens (performCheckOnly :: UpgradeElasticsearchDomain -> Lude.Maybe Lude.Bool) (\s a -> s {performCheckOnly = a} :: UpgradeElasticsearchDomain)
{-# DEPRECATED uedPerformCheckOnly "Use generic-lens or generic-optics with 'performCheckOnly' instead." #-}

-- | The version of Elasticsearch that you intend to upgrade the domain to.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedTargetVersion :: Lens.Lens' UpgradeElasticsearchDomain Lude.Text
uedTargetVersion = Lens.lens (targetVersion :: UpgradeElasticsearchDomain -> Lude.Text) (\s a -> s {targetVersion = a} :: UpgradeElasticsearchDomain)
{-# DEPRECATED uedTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

instance Lude.AWSRequest UpgradeElasticsearchDomain where
  type
    Rs UpgradeElasticsearchDomain =
      UpgradeElasticsearchDomainResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpgradeElasticsearchDomainResponse'
            Lude.<$> (x Lude..?> "DomainName")
            Lude.<*> (x Lude..?> "PerformCheckOnly")
            Lude.<*> (x Lude..?> "TargetVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpgradeElasticsearchDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpgradeElasticsearchDomain where
  toJSON UpgradeElasticsearchDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            ("PerformCheckOnly" Lude..=) Lude.<$> performCheckOnly,
            Lude.Just ("TargetVersion" Lude..= targetVersion)
          ]
      )

instance Lude.ToPath UpgradeElasticsearchDomain where
  toPath = Lude.const "/2015-01-01/es/upgradeDomain"

instance Lude.ToQuery UpgradeElasticsearchDomain where
  toQuery = Lude.const Lude.mempty

-- | Container for response returned by @'UpgradeElasticsearchDomain' @ operation.
--
-- /See:/ 'mkUpgradeElasticsearchDomainResponse' smart constructor.
data UpgradeElasticsearchDomainResponse = UpgradeElasticsearchDomainResponse'
  { domainName :: Lude.Maybe Lude.Text,
    -- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Lude.Maybe Lude.Bool,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainName' -
-- * 'performCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
-- * 'targetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
-- * 'responseStatus' - The response status code.
mkUpgradeElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpgradeElasticsearchDomainResponse
mkUpgradeElasticsearchDomainResponse pResponseStatus_ =
  UpgradeElasticsearchDomainResponse'
    { domainName = Lude.Nothing,
      performCheckOnly = Lude.Nothing,
      targetVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrsDomainName :: Lens.Lens' UpgradeElasticsearchDomainResponse (Lude.Maybe Lude.Text)
uedrsDomainName = Lens.lens (domainName :: UpgradeElasticsearchDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: UpgradeElasticsearchDomainResponse)
{-# DEPRECATED uedrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
--
-- /Note:/ Consider using 'performCheckOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrsPerformCheckOnly :: Lens.Lens' UpgradeElasticsearchDomainResponse (Lude.Maybe Lude.Bool)
uedrsPerformCheckOnly = Lens.lens (performCheckOnly :: UpgradeElasticsearchDomainResponse -> Lude.Maybe Lude.Bool) (\s a -> s {performCheckOnly = a} :: UpgradeElasticsearchDomainResponse)
{-# DEPRECATED uedrsPerformCheckOnly "Use generic-lens or generic-optics with 'performCheckOnly' instead." #-}

-- | The version of Elasticsearch that you intend to upgrade the domain to.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrsTargetVersion :: Lens.Lens' UpgradeElasticsearchDomainResponse (Lude.Maybe Lude.Text)
uedrsTargetVersion = Lens.lens (targetVersion :: UpgradeElasticsearchDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetVersion = a} :: UpgradeElasticsearchDomainResponse)
{-# DEPRECATED uedrsTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrsResponseStatus :: Lens.Lens' UpgradeElasticsearchDomainResponse Lude.Int
uedrsResponseStatus = Lens.lens (responseStatus :: UpgradeElasticsearchDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpgradeElasticsearchDomainResponse)
{-# DEPRECATED uedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
