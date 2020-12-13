{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListOnPremisesInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of names for one or more on-premises instances.
--
-- Unless otherwise specified, both registered and deregistered on-premises instance names are listed. To list only registered or deregistered on-premises instance names, use the registration status parameter.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListOnPremisesInstances
  ( -- * Creating a request
    ListOnPremisesInstances (..),
    mkListOnPremisesInstances,

    -- ** Request lenses
    lopiTagFilters,
    lopiNextToken,
    lopiRegistrationStatus,

    -- * Destructuring the response
    ListOnPremisesInstancesResponse (..),
    mkListOnPremisesInstancesResponse,

    -- ** Response lenses
    lopirsNextToken,
    lopirsInstanceNames,
    lopirsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListOnPremisesInstances@ operation.
--
-- /See:/ 'mkListOnPremisesInstances' smart constructor.
data ListOnPremisesInstances = ListOnPremisesInstances'
  { -- | The on-premises instance tags that are used to restrict the on-premises instance names returned.
    tagFilters :: Lude.Maybe [TagFilter],
    -- | An identifier returned from the previous list on-premises instances call. It can be used to return the next set of on-premises instances in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The registration status of the on-premises instances:
    --
    --
    --     * @Deregistered@ : Include deregistered on-premises instances in the resulting list.
    --
    --
    --     * @Registered@ : Include registered on-premises instances in the resulting list.
    registrationStatus :: Lude.Maybe RegistrationStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOnPremisesInstances' with the minimum fields required to make a request.
--
-- * 'tagFilters' - The on-premises instance tags that are used to restrict the on-premises instance names returned.
-- * 'nextToken' - An identifier returned from the previous list on-premises instances call. It can be used to return the next set of on-premises instances in the list.
-- * 'registrationStatus' - The registration status of the on-premises instances:
--
--
--     * @Deregistered@ : Include deregistered on-premises instances in the resulting list.
--
--
--     * @Registered@ : Include registered on-premises instances in the resulting list.
mkListOnPremisesInstances ::
  ListOnPremisesInstances
mkListOnPremisesInstances =
  ListOnPremisesInstances'
    { tagFilters = Lude.Nothing,
      nextToken = Lude.Nothing,
      registrationStatus = Lude.Nothing
    }

-- | The on-premises instance tags that are used to restrict the on-premises instance names returned.
--
-- /Note:/ Consider using 'tagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopiTagFilters :: Lens.Lens' ListOnPremisesInstances (Lude.Maybe [TagFilter])
lopiTagFilters = Lens.lens (tagFilters :: ListOnPremisesInstances -> Lude.Maybe [TagFilter]) (\s a -> s {tagFilters = a} :: ListOnPremisesInstances)
{-# DEPRECATED lopiTagFilters "Use generic-lens or generic-optics with 'tagFilters' instead." #-}

-- | An identifier returned from the previous list on-premises instances call. It can be used to return the next set of on-premises instances in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopiNextToken :: Lens.Lens' ListOnPremisesInstances (Lude.Maybe Lude.Text)
lopiNextToken = Lens.lens (nextToken :: ListOnPremisesInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOnPremisesInstances)
{-# DEPRECATED lopiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The registration status of the on-premises instances:
--
--
--     * @Deregistered@ : Include deregistered on-premises instances in the resulting list.
--
--
--     * @Registered@ : Include registered on-premises instances in the resulting list.
--
--
--
-- /Note:/ Consider using 'registrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopiRegistrationStatus :: Lens.Lens' ListOnPremisesInstances (Lude.Maybe RegistrationStatus)
lopiRegistrationStatus = Lens.lens (registrationStatus :: ListOnPremisesInstances -> Lude.Maybe RegistrationStatus) (\s a -> s {registrationStatus = a} :: ListOnPremisesInstances)
{-# DEPRECATED lopiRegistrationStatus "Use generic-lens or generic-optics with 'registrationStatus' instead." #-}

instance Page.AWSPager ListOnPremisesInstances where
  page rq rs
    | Page.stop (rs Lens.^. lopirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lopirsInstanceNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lopiNextToken Lens..~ rs Lens.^. lopirsNextToken

instance Lude.AWSRequest ListOnPremisesInstances where
  type Rs ListOnPremisesInstances = ListOnPremisesInstancesResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOnPremisesInstancesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "instanceNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOnPremisesInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListOnPremisesInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOnPremisesInstances where
  toJSON ListOnPremisesInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tagFilters" Lude..=) Lude.<$> tagFilters,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("registrationStatus" Lude..=) Lude.<$> registrationStatus
          ]
      )

instance Lude.ToPath ListOnPremisesInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOnPremisesInstances where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of the list on-premises instances operation.
--
-- /See:/ 'mkListOnPremisesInstancesResponse' smart constructor.
data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse'
  { -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list on-premises instances call to return the next set of on-premises instances in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of matching on-premises instance names.
    instanceNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOnPremisesInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list on-premises instances call to return the next set of on-premises instances in the list.
-- * 'instanceNames' - The list of matching on-premises instance names.
-- * 'responseStatus' - The response status code.
mkListOnPremisesInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOnPremisesInstancesResponse
mkListOnPremisesInstancesResponse pResponseStatus_ =
  ListOnPremisesInstancesResponse'
    { nextToken = Lude.Nothing,
      instanceNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list on-premises instances call to return the next set of on-premises instances in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopirsNextToken :: Lens.Lens' ListOnPremisesInstancesResponse (Lude.Maybe Lude.Text)
lopirsNextToken = Lens.lens (nextToken :: ListOnPremisesInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOnPremisesInstancesResponse)
{-# DEPRECATED lopirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of matching on-premises instance names.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopirsInstanceNames :: Lens.Lens' ListOnPremisesInstancesResponse (Lude.Maybe [Lude.Text])
lopirsInstanceNames = Lens.lens (instanceNames :: ListOnPremisesInstancesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceNames = a} :: ListOnPremisesInstancesResponse)
{-# DEPRECATED lopirsInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopirsResponseStatus :: Lens.Lens' ListOnPremisesInstancesResponse Lude.Int
lopirsResponseStatus = Lens.lens (responseStatus :: ListOnPremisesInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOnPremisesInstancesResponse)
{-# DEPRECATED lopirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
