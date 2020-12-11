{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the access rules that control access to the domain's document and search endpoints. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for an Amazon CloudSearch Domain> .
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
  ( -- * Creating a request
    UpdateServiceAccessPolicies (..),
    mkUpdateServiceAccessPolicies,

    -- ** Request lenses
    usapDomainName,
    usapAccessPolicies,

    -- * Destructuring the response
    UpdateServiceAccessPoliciesResponse (..),
    mkUpdateServiceAccessPoliciesResponse,

    -- ** Response lenses
    usaprsResponseStatus,
    usaprsAccessPolicies,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'UpdateServiceAccessPolicies' @ operation. Specifies the name of the domain you want to update and the access rules you want to configure.
--
-- /See:/ 'mkUpdateServiceAccessPolicies' smart constructor.
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
  { domainName ::
      Lude.Text,
    accessPolicies :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceAccessPolicies' with the minimum fields required to make a request.
--
-- * 'accessPolicies' - The access rules you want to configure. These rules replace any existing rules.
-- * 'domainName' - Undocumented field.
mkUpdateServiceAccessPolicies ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'accessPolicies'
  Lude.Text ->
  UpdateServiceAccessPolicies
mkUpdateServiceAccessPolicies pDomainName_ pAccessPolicies_ =
  UpdateServiceAccessPolicies'
    { domainName = pDomainName_,
      accessPolicies = pAccessPolicies_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usapDomainName :: Lens.Lens' UpdateServiceAccessPolicies Lude.Text
usapDomainName = Lens.lens (domainName :: UpdateServiceAccessPolicies -> Lude.Text) (\s a -> s {domainName = a} :: UpdateServiceAccessPolicies)
{-# DEPRECATED usapDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The access rules you want to configure. These rules replace any existing rules.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usapAccessPolicies :: Lens.Lens' UpdateServiceAccessPolicies Lude.Text
usapAccessPolicies = Lens.lens (accessPolicies :: UpdateServiceAccessPolicies -> Lude.Text) (\s a -> s {accessPolicies = a} :: UpdateServiceAccessPolicies)
{-# DEPRECATED usapAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

instance Lude.AWSRequest UpdateServiceAccessPolicies where
  type
    Rs UpdateServiceAccessPolicies =
      UpdateServiceAccessPoliciesResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "UpdateServiceAccessPoliciesResult"
      ( \s h x ->
          UpdateServiceAccessPoliciesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "AccessPolicies")
      )

instance Lude.ToHeaders UpdateServiceAccessPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateServiceAccessPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServiceAccessPolicies where
  toQuery UpdateServiceAccessPolicies' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateServiceAccessPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "AccessPolicies" Lude.=: accessPolicies
      ]

-- | The result of an @UpdateServiceAccessPolicies@ request. Contains the new access policies.
--
-- /See:/ 'mkUpdateServiceAccessPoliciesResponse' smart constructor.
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
  { responseStatus ::
      Lude.Int,
    accessPolicies ::
      AccessPoliciesStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceAccessPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'accessPolicies' - The access rules configured for the domain.
-- * 'responseStatus' - The response status code.
mkUpdateServiceAccessPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'accessPolicies'
  AccessPoliciesStatus ->
  UpdateServiceAccessPoliciesResponse
mkUpdateServiceAccessPoliciesResponse
  pResponseStatus_
  pAccessPolicies_ =
    UpdateServiceAccessPoliciesResponse'
      { responseStatus =
          pResponseStatus_,
        accessPolicies = pAccessPolicies_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaprsResponseStatus :: Lens.Lens' UpdateServiceAccessPoliciesResponse Lude.Int
usaprsResponseStatus = Lens.lens (responseStatus :: UpdateServiceAccessPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServiceAccessPoliciesResponse)
{-# DEPRECATED usaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The access rules configured for the domain.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaprsAccessPolicies :: Lens.Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
usaprsAccessPolicies = Lens.lens (accessPolicies :: UpdateServiceAccessPoliciesResponse -> AccessPoliciesStatus) (\s a -> s {accessPolicies = a} :: UpdateServiceAccessPoliciesResponse)
{-# DEPRECATED usaprsAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}
