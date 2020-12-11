{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS organization. The account whose user is calling the @CreateOrganization@ operation automatically becomes the <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account management account> of the new organization.
--
-- This operation must be called using credentials from the account that is to become the new organization's management account. The principal must also have the relevant IAM permissions.
-- By default (or if you set the @FeatureSet@ parameter to @ALL@ ), the new organization is created with all features enabled and service control policies automatically enabled in the root. If you instead choose to create the organization supporting only the consolidated billing features by setting the @FeatureSet@ parameter to @CONSOLIDATED_BILLING"@ , no policy types are enabled by default, and you can't use organization policies
module Network.AWS.Organizations.CreateOrganization
  ( -- * Creating a request
    CreateOrganization (..),
    mkCreateOrganization,

    -- ** Request lenses
    coFeatureSet,

    -- * Destructuring the response
    CreateOrganizationResponse (..),
    mkCreateOrganizationResponse,

    -- ** Response lenses
    corsOrganization,
    corsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateOrganization' smart constructor.
newtype CreateOrganization = CreateOrganization'
  { featureSet ::
      Lude.Maybe OrganizationFeatureSet
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrganization' with the minimum fields required to make a request.
--
-- * 'featureSet' - Specifies the feature set supported by the new organization. Each feature set supports different levels of functionality.
--
--
--     * @CONSOLIDATED_BILLING@ : All member accounts have their bills consolidated to and paid by the management account. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing> in the /AWS Organizations User Guide./
-- The consolidated billing feature subset isn't available for organizations in the AWS GovCloud (US) Region.
--
--
--     * @ALL@ : In addition to all the features supported by the consolidated billing feature set, the management account can also apply any policy type to any member account in the organization. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features> in the /AWS Organizations User Guide./
mkCreateOrganization ::
  CreateOrganization
mkCreateOrganization =
  CreateOrganization' {featureSet = Lude.Nothing}

-- | Specifies the feature set supported by the new organization. Each feature set supports different levels of functionality.
--
--
--     * @CONSOLIDATED_BILLING@ : All member accounts have their bills consolidated to and paid by the management account. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing> in the /AWS Organizations User Guide./
-- The consolidated billing feature subset isn't available for organizations in the AWS GovCloud (US) Region.
--
--
--     * @ALL@ : In addition to all the features supported by the consolidated billing feature set, the management account can also apply any policy type to any member account in the organization. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features> in the /AWS Organizations User Guide./
--
--
--
-- /Note:/ Consider using 'featureSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coFeatureSet :: Lens.Lens' CreateOrganization (Lude.Maybe OrganizationFeatureSet)
coFeatureSet = Lens.lens (featureSet :: CreateOrganization -> Lude.Maybe OrganizationFeatureSet) (\s a -> s {featureSet = a} :: CreateOrganization)
{-# DEPRECATED coFeatureSet "Use generic-lens or generic-optics with 'featureSet' instead." #-}

instance Lude.AWSRequest CreateOrganization where
  type Rs CreateOrganization = CreateOrganizationResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOrganizationResponse'
            Lude.<$> (x Lude..?> "Organization") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.CreateOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateOrganization where
  toJSON CreateOrganization' {..} =
    Lude.object
      (Lude.catMaybes [("FeatureSet" Lude..=) Lude.<$> featureSet])

instance Lude.ToPath CreateOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { organization ::
      Lude.Maybe Organization,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'organization' - A structure that contains details about the newly created organization.
-- * 'responseStatus' - The response status code.
mkCreateOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOrganizationResponse
mkCreateOrganizationResponse pResponseStatus_ =
  CreateOrganizationResponse'
    { organization = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the newly created organization.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsOrganization :: Lens.Lens' CreateOrganizationResponse (Lude.Maybe Organization)
corsOrganization = Lens.lens (organization :: CreateOrganizationResponse -> Lude.Maybe Organization) (\s a -> s {organization = a} :: CreateOrganizationResponse)
{-# DEPRECATED corsOrganization "Use generic-lens or generic-optics with 'organization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsResponseStatus :: Lens.Lens' CreateOrganizationResponse Lude.Int
corsResponseStatus = Lens.lens (responseStatus :: CreateOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOrganizationResponse)
{-# DEPRECATED corsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
