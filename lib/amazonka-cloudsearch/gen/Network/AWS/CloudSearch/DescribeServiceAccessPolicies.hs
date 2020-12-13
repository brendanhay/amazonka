{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the access policies that control access to the domain's document and search endpoints. By default, shows the configuration with any pending changes. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeServiceAccessPolicies
  ( -- * Creating a request
    DescribeServiceAccessPolicies (..),
    mkDescribeServiceAccessPolicies,

    -- ** Request lenses
    dsapDeployed,
    dsapDomainName,

    -- * Destructuring the response
    DescribeServiceAccessPoliciesResponse (..),
    mkDescribeServiceAccessPoliciesResponse,

    -- ** Response lenses
    dsaprsAccessPolicies,
    dsaprsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeServiceAccessPolicies' @ operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeServiceAccessPolicies' smart constructor.
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'
  { -- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
    deployed :: Lude.Maybe Lude.Bool,
    -- | The name of the domain you want to describe.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceAccessPolicies' with the minimum fields required to make a request.
--
-- * 'deployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
-- * 'domainName' - The name of the domain you want to describe.
mkDescribeServiceAccessPolicies ::
  -- | 'domainName'
  Lude.Text ->
  DescribeServiceAccessPolicies
mkDescribeServiceAccessPolicies pDomainName_ =
  DescribeServiceAccessPolicies'
    { deployed = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsapDeployed :: Lens.Lens' DescribeServiceAccessPolicies (Lude.Maybe Lude.Bool)
dsapDeployed = Lens.lens (deployed :: DescribeServiceAccessPolicies -> Lude.Maybe Lude.Bool) (\s a -> s {deployed = a} :: DescribeServiceAccessPolicies)
{-# DEPRECATED dsapDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsapDomainName :: Lens.Lens' DescribeServiceAccessPolicies Lude.Text
dsapDomainName = Lens.lens (domainName :: DescribeServiceAccessPolicies -> Lude.Text) (\s a -> s {domainName = a} :: DescribeServiceAccessPolicies)
{-# DEPRECATED dsapDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeServiceAccessPolicies where
  type
    Rs DescribeServiceAccessPolicies =
      DescribeServiceAccessPoliciesResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeServiceAccessPoliciesResult"
      ( \s h x ->
          DescribeServiceAccessPoliciesResponse'
            Lude.<$> (x Lude..@ "AccessPolicies")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServiceAccessPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeServiceAccessPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServiceAccessPolicies where
  toQuery DescribeServiceAccessPolicies' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeServiceAccessPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "Deployed" Lude.=: deployed,
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeServiceAccessPolicies@ request.
--
-- /See:/ 'mkDescribeServiceAccessPoliciesResponse' smart constructor.
data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse'
  { -- | The access rules configured for the domain specified in the request.
    accessPolicies :: AccessPoliciesStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceAccessPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'accessPolicies' - The access rules configured for the domain specified in the request.
-- * 'responseStatus' - The response status code.
mkDescribeServiceAccessPoliciesResponse ::
  -- | 'accessPolicies'
  AccessPoliciesStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServiceAccessPoliciesResponse
mkDescribeServiceAccessPoliciesResponse
  pAccessPolicies_
  pResponseStatus_ =
    DescribeServiceAccessPoliciesResponse'
      { accessPolicies =
          pAccessPolicies_,
        responseStatus = pResponseStatus_
      }

-- | The access rules configured for the domain specified in the request.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaprsAccessPolicies :: Lens.Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprsAccessPolicies = Lens.lens (accessPolicies :: DescribeServiceAccessPoliciesResponse -> AccessPoliciesStatus) (\s a -> s {accessPolicies = a} :: DescribeServiceAccessPoliciesResponse)
{-# DEPRECATED dsaprsAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaprsResponseStatus :: Lens.Lens' DescribeServiceAccessPoliciesResponse Lude.Int
dsaprsResponseStatus = Lens.lens (responseStatus :: DescribeServiceAccessPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServiceAccessPoliciesResponse)
{-# DEPRECATED dsaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
