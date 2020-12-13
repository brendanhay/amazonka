{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetUpgradeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the latest status of the last upgrade or upgrade eligibility check that was performed on the domain.
module Network.AWS.ElasticSearch.GetUpgradeStatus
  ( -- * Creating a request
    GetUpgradeStatus (..),
    mkGetUpgradeStatus,

    -- ** Request lenses
    gusDomainName,

    -- * Destructuring the response
    GetUpgradeStatusResponse (..),
    mkGetUpgradeStatusResponse,

    -- ** Response lenses
    gusrsStepStatus,
    gusrsUpgradeName,
    gusrsUpgradeStep,
    gusrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'GetUpgradeStatus' @ operation.
--
-- /See:/ 'mkGetUpgradeStatus' smart constructor.
newtype GetUpgradeStatus = GetUpgradeStatus'
  { domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUpgradeStatus' with the minimum fields required to make a request.
--
-- * 'domainName' -
mkGetUpgradeStatus ::
  -- | 'domainName'
  Lude.Text ->
  GetUpgradeStatus
mkGetUpgradeStatus pDomainName_ =
  GetUpgradeStatus' {domainName = pDomainName_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusDomainName :: Lens.Lens' GetUpgradeStatus Lude.Text
gusDomainName = Lens.lens (domainName :: GetUpgradeStatus -> Lude.Text) (\s a -> s {domainName = a} :: GetUpgradeStatus)
{-# DEPRECATED gusDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest GetUpgradeStatus where
  type Rs GetUpgradeStatus = GetUpgradeStatusResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUpgradeStatusResponse'
            Lude.<$> (x Lude..?> "StepStatus")
            Lude.<*> (x Lude..?> "UpgradeName")
            Lude.<*> (x Lude..?> "UpgradeStep")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUpgradeStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetUpgradeStatus where
  toPath GetUpgradeStatus' {..} =
    Lude.mconcat
      ["/2015-01-01/es/upgradeDomain/", Lude.toBS domainName, "/status"]

instance Lude.ToQuery GetUpgradeStatus where
  toQuery = Lude.const Lude.mempty

-- | Container for response returned by @'GetUpgradeStatus' @ operation.
--
-- /See:/ 'mkGetUpgradeStatusResponse' smart constructor.
data GetUpgradeStatusResponse = GetUpgradeStatusResponse'
  { -- | One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:
    --
    --     * In Progress
    --
    --     * Succeeded
    --
    --     * Succeeded with Issues
    --
    --     * Failed
    stepStatus :: Lude.Maybe UpgradeStatus,
    -- | A string that describes the update briefly
    upgradeName :: Lude.Maybe Lude.Text,
    -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
    --
    --     * PreUpgradeCheck
    --
    --     * Snapshot
    --
    --     * Upgrade
    upgradeStep :: Lude.Maybe UpgradeStep,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUpgradeStatusResponse' with the minimum fields required to make a request.
--
-- * 'stepStatus' - One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:
--
--     * In Progress
--
--     * Succeeded
--
--     * Succeeded with Issues
--
--     * Failed
--
--
-- * 'upgradeName' - A string that describes the update briefly
-- * 'upgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
--
--     * PreUpgradeCheck
--
--     * Snapshot
--
--     * Upgrade
--
--
-- * 'responseStatus' - The response status code.
mkGetUpgradeStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUpgradeStatusResponse
mkGetUpgradeStatusResponse pResponseStatus_ =
  GetUpgradeStatusResponse'
    { stepStatus = Lude.Nothing,
      upgradeName = Lude.Nothing,
      upgradeStep = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:
--
--     * In Progress
--
--     * Succeeded
--
--     * Succeeded with Issues
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'stepStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsStepStatus :: Lens.Lens' GetUpgradeStatusResponse (Lude.Maybe UpgradeStatus)
gusrsStepStatus = Lens.lens (stepStatus :: GetUpgradeStatusResponse -> Lude.Maybe UpgradeStatus) (\s a -> s {stepStatus = a} :: GetUpgradeStatusResponse)
{-# DEPRECATED gusrsStepStatus "Use generic-lens or generic-optics with 'stepStatus' instead." #-}

-- | A string that describes the update briefly
--
-- /Note:/ Consider using 'upgradeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsUpgradeName :: Lens.Lens' GetUpgradeStatusResponse (Lude.Maybe Lude.Text)
gusrsUpgradeName = Lens.lens (upgradeName :: GetUpgradeStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {upgradeName = a} :: GetUpgradeStatusResponse)
{-# DEPRECATED gusrsUpgradeName "Use generic-lens or generic-optics with 'upgradeName' instead." #-}

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
--
--     * PreUpgradeCheck
--
--     * Snapshot
--
--     * Upgrade
--
--
--
-- /Note:/ Consider using 'upgradeStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsUpgradeStep :: Lens.Lens' GetUpgradeStatusResponse (Lude.Maybe UpgradeStep)
gusrsUpgradeStep = Lens.lens (upgradeStep :: GetUpgradeStatusResponse -> Lude.Maybe UpgradeStep) (\s a -> s {upgradeStep = a} :: GetUpgradeStatusResponse)
{-# DEPRECATED gusrsUpgradeStep "Use generic-lens or generic-optics with 'upgradeStep' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsResponseStatus :: Lens.Lens' GetUpgradeStatusResponse Lude.Int
gusrsResponseStatus = Lens.lens (responseStatus :: GetUpgradeStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUpgradeStatusResponse)
{-# DEPRECATED gusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
