{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified provisioning artifact (also known as a version) for the specified product.
--
-- You cannot update a provisioning artifact for a product that was shared with you.
module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
  ( -- * Creating a request
    UpdateProvisioningArtifact (..),
    mkUpdateProvisioningArtifact,

    -- ** Request lenses
    upaProvisioningArtifactId,
    upaActive,
    upaName,
    upaAcceptLanguage,
    upaGuidance,
    upaDescription,
    upaProductId,

    -- * Destructuring the response
    UpdateProvisioningArtifactResponse (..),
    mkUpdateProvisioningArtifactResponse,

    -- ** Response lenses
    uparsStatus,
    uparsInfo,
    uparsProvisioningArtifactDetail,
    uparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateProvisioningArtifact' smart constructor.
data UpdateProvisioningArtifact = UpdateProvisioningArtifact'
  { -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Lude.Text,
    -- | Indicates whether the product version is active.
    --
    -- Inactive provisioning artifacts are invisible to end users. End users cannot launch or update a provisioned product from an inactive provisioning artifact.
    active :: Lude.Maybe Lude.Bool,
    -- | The updated name of the provisioning artifact.
    name :: Lude.Maybe Lude.Text,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
    --
    -- The @DEFAULT@ value indicates that the product version is active.
    -- The administrator can set the guidance to @DEPRECATED@ to inform users that the product version is deprecated. Users are able to make updates to a provisioned product of a deprecated version but cannot launch new provisioned products using a deprecated version.
    guidance :: Lude.Maybe ProvisioningArtifactGuidance,
    -- | The updated description of the provisioning artifact.
    description :: Lude.Maybe Lude.Text,
    -- | The product identifier.
    productId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisioningArtifact' with the minimum fields required to make a request.
--
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
-- * 'active' - Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users cannot launch or update a provisioned product from an inactive provisioning artifact.
-- * 'name' - The updated name of the provisioning artifact.
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'guidance' - Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- The @DEFAULT@ value indicates that the product version is active.
-- The administrator can set the guidance to @DEPRECATED@ to inform users that the product version is deprecated. Users are able to make updates to a provisioned product of a deprecated version but cannot launch new provisioned products using a deprecated version.
-- * 'description' - The updated description of the provisioning artifact.
-- * 'productId' - The product identifier.
mkUpdateProvisioningArtifact ::
  -- | 'provisioningArtifactId'
  Lude.Text ->
  -- | 'productId'
  Lude.Text ->
  UpdateProvisioningArtifact
mkUpdateProvisioningArtifact pProvisioningArtifactId_ pProductId_ =
  UpdateProvisioningArtifact'
    { provisioningArtifactId =
        pProvisioningArtifactId_,
      active = Lude.Nothing,
      name = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      guidance = Lude.Nothing,
      description = Lude.Nothing,
      productId = pProductId_
    }

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaProvisioningArtifactId :: Lens.Lens' UpdateProvisioningArtifact Lude.Text
upaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: UpdateProvisioningArtifact -> Lude.Text) (\s a -> s {provisioningArtifactId = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users cannot launch or update a provisioned product from an inactive provisioning artifact.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaActive :: Lens.Lens' UpdateProvisioningArtifact (Lude.Maybe Lude.Bool)
upaActive = Lens.lens (active :: UpdateProvisioningArtifact -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The updated name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaName :: Lens.Lens' UpdateProvisioningArtifact (Lude.Maybe Lude.Text)
upaName = Lens.lens (name :: UpdateProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaAcceptLanguage :: Lens.Lens' UpdateProvisioningArtifact (Lude.Maybe Lude.Text)
upaAcceptLanguage = Lens.lens (acceptLanguage :: UpdateProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- The @DEFAULT@ value indicates that the product version is active.
-- The administrator can set the guidance to @DEPRECATED@ to inform users that the product version is deprecated. Users are able to make updates to a provisioned product of a deprecated version but cannot launch new provisioned products using a deprecated version.
--
-- /Note:/ Consider using 'guidance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaGuidance :: Lens.Lens' UpdateProvisioningArtifact (Lude.Maybe ProvisioningArtifactGuidance)
upaGuidance = Lens.lens (guidance :: UpdateProvisioningArtifact -> Lude.Maybe ProvisioningArtifactGuidance) (\s a -> s {guidance = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaGuidance "Use generic-lens or generic-optics with 'guidance' instead." #-}

-- | The updated description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaDescription :: Lens.Lens' UpdateProvisioningArtifact (Lude.Maybe Lude.Text)
upaDescription = Lens.lens (description :: UpdateProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaProductId :: Lens.Lens' UpdateProvisioningArtifact Lude.Text
upaProductId = Lens.lens (productId :: UpdateProvisioningArtifact -> Lude.Text) (\s a -> s {productId = a} :: UpdateProvisioningArtifact)
{-# DEPRECATED upaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.AWSRequest UpdateProvisioningArtifact where
  type
    Rs UpdateProvisioningArtifact =
      UpdateProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProvisioningArtifactResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Info" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ProvisioningArtifactDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProvisioningArtifact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.UpdateProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProvisioningArtifact where
  toJSON UpdateProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ProvisioningArtifactId" Lude..= provisioningArtifactId),
            ("Active" Lude..=) Lude.<$> active,
            ("Name" Lude..=) Lude.<$> name,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Guidance" Lude..=) Lude.<$> guidance,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )

instance Lude.ToPath UpdateProvisioningArtifact where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProvisioningArtifact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProvisioningArtifactResponse' smart constructor.
data UpdateProvisioningArtifactResponse = UpdateProvisioningArtifactResponse'
  { -- | The status of the current request.
    status :: Lude.Maybe RequestStatus,
    -- | The URL of the CloudFormation template in Amazon S3.
    info :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Lude.Maybe ProvisioningArtifactDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the current request.
-- * 'info' - The URL of the CloudFormation template in Amazon S3.
-- * 'provisioningArtifactDetail' - Information about the provisioning artifact.
-- * 'responseStatus' - The response status code.
mkUpdateProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProvisioningArtifactResponse
mkUpdateProvisioningArtifactResponse pResponseStatus_ =
  UpdateProvisioningArtifactResponse'
    { status = Lude.Nothing,
      info = Lude.Nothing,
      provisioningArtifactDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparsStatus :: Lens.Lens' UpdateProvisioningArtifactResponse (Lude.Maybe RequestStatus)
uparsStatus = Lens.lens (status :: UpdateProvisioningArtifactResponse -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: UpdateProvisioningArtifactResponse)
{-# DEPRECATED uparsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The URL of the CloudFormation template in Amazon S3.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparsInfo :: Lens.Lens' UpdateProvisioningArtifactResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uparsInfo = Lens.lens (info :: UpdateProvisioningArtifactResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {info = a} :: UpdateProvisioningArtifactResponse)
{-# DEPRECATED uparsInfo "Use generic-lens or generic-optics with 'info' instead." #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparsProvisioningArtifactDetail :: Lens.Lens' UpdateProvisioningArtifactResponse (Lude.Maybe ProvisioningArtifactDetail)
uparsProvisioningArtifactDetail = Lens.lens (provisioningArtifactDetail :: UpdateProvisioningArtifactResponse -> Lude.Maybe ProvisioningArtifactDetail) (\s a -> s {provisioningArtifactDetail = a} :: UpdateProvisioningArtifactResponse)
{-# DEPRECATED uparsProvisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparsResponseStatus :: Lens.Lens' UpdateProvisioningArtifactResponse Lude.Int
uparsResponseStatus = Lens.lens (responseStatus :: UpdateProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProvisioningArtifactResponse)
{-# DEPRECATED uparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
