{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the configuration required to provision the specified product using the specified provisioning artifact.
--
-- If the output contains a TagOption key with an empty list of values, there is a TagOption conflict for that key. The end user cannot take action to fix the conflict, and launch is not blocked. In subsequent calls to 'ProvisionProduct' , do not include conflicted TagOption keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ ". Tag the provisioned product with the value @sc-tagoption-conflict-portfolioId-productId@ .
module Network.AWS.ServiceCatalog.DescribeProvisioningParameters
  ( -- * Creating a request
    DescribeProvisioningParameters (..),
    mkDescribeProvisioningParameters,

    -- ** Request lenses
    dppsProductName,
    dppsProvisioningArtifactId,
    dppsProvisioningArtifactName,
    dppsPathName,
    dppsAcceptLanguage,
    dppsPathId,
    dppsProductId,

    -- * Destructuring the response
    DescribeProvisioningParametersResponse (..),
    mkDescribeProvisioningParametersResponse,

    -- ** Response lenses
    dpprsProvisioningArtifactPreferences,
    dpprsProvisioningArtifactParameters,
    dpprsUsageInstructions,
    dpprsConstraintSummaries,
    dpprsTagOptions,
    dpprsProvisioningArtifactOutputs,
    dpprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeProvisioningParameters' smart constructor.
data DescribeProvisioningParameters = DescribeProvisioningParameters'
  { productName ::
      Lude.Maybe Lude.Text,
    provisioningArtifactId ::
      Lude.Maybe Lude.Text,
    provisioningArtifactName ::
      Lude.Maybe Lude.Text,
    pathName ::
      Lude.Maybe Lude.Text,
    acceptLanguage ::
      Lude.Maybe Lude.Text,
    pathId ::
      Lude.Maybe Lude.Text,
    productId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningParameters' with the minimum fields required to make a request.
--
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
-- * 'pathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
-- * 'pathName' - The name of the path. You must provide the name or ID, but not both.
-- * 'productId' - The product identifier. You must provide the product name or ID, but not both.
-- * 'productName' - The name of the product. You must provide the name or ID, but not both.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. You must provide the name or ID, but not both.
-- * 'provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID, but not both.
mkDescribeProvisioningParameters ::
  DescribeProvisioningParameters
mkDescribeProvisioningParameters =
  DescribeProvisioningParameters'
    { productName = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      provisioningArtifactName = Lude.Nothing,
      pathName = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pathId = Lude.Nothing,
      productId = Lude.Nothing
    }

-- | The name of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppsProductName :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsProductName = Lens.lens (productName :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {productName = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

-- | The identifier of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppsProvisioningArtifactId :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsProvisioningArtifactId = Lens.lens (provisioningArtifactId :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppsProvisioningArtifactName :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsProvisioningArtifactName = Lens.lens (provisioningArtifactName :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactName = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

-- | The name of the path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppsPathName :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsPathName = Lens.lens (pathName :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {pathName = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsPathName "Use generic-lens or generic-optics with 'pathName' instead." #-}

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
dppsAcceptLanguage :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppsPathId :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsPathId = Lens.lens (pathId :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {pathId = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | The product identifier. You must provide the product name or ID, but not both.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppsProductId :: Lens.Lens' DescribeProvisioningParameters (Lude.Maybe Lude.Text)
dppsProductId = Lens.lens (productId :: DescribeProvisioningParameters -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: DescribeProvisioningParameters)
{-# DEPRECATED dppsProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.AWSRequest DescribeProvisioningParameters where
  type
    Rs DescribeProvisioningParameters =
      DescribeProvisioningParametersResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProvisioningParametersResponse'
            Lude.<$> (x Lude..?> "ProvisioningArtifactPreferences")
            Lude.<*> (x Lude..?> "ProvisioningArtifactParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UsageInstructions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ConstraintSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TagOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ProvisioningArtifactOutputs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProvisioningParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeProvisioningParameters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProvisioningParameters where
  toJSON DescribeProvisioningParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProductName" Lude..=) Lude.<$> productName,
            ("ProvisioningArtifactId" Lude..=) Lude.<$> provisioningArtifactId,
            ("ProvisioningArtifactName" Lude..=)
              Lude.<$> provisioningArtifactName,
            ("PathName" Lude..=) Lude.<$> pathName,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PathId" Lude..=) Lude.<$> pathId,
            ("ProductId" Lude..=) Lude.<$> productId
          ]
      )

instance Lude.ToPath DescribeProvisioningParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProvisioningParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProvisioningParametersResponse' smart constructor.
data DescribeProvisioningParametersResponse = DescribeProvisioningParametersResponse'
  { provisioningArtifactPreferences ::
      Lude.Maybe
        ProvisioningArtifactPreferences,
    provisioningArtifactParameters ::
      Lude.Maybe
        [ProvisioningArtifactParameter],
    usageInstructions ::
      Lude.Maybe
        [UsageInstruction],
    constraintSummaries ::
      Lude.Maybe
        [ConstraintSummary],
    tagOptions ::
      Lude.Maybe
        [TagOptionSummary],
    provisioningArtifactOutputs ::
      Lude.Maybe
        [ProvisioningArtifactOutput],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningParametersResponse' with the minimum fields required to make a request.
--
-- * 'constraintSummaries' - Information about the constraints used to provision the product.
-- * 'provisioningArtifactOutputs' - The output of the provisioning artifact.
-- * 'provisioningArtifactParameters' - Information about the parameters used to provision the product.
-- * 'provisioningArtifactPreferences' - An object that contains information about preferences, such as regions and accounts, for the provisioning artifact.
-- * 'responseStatus' - The response status code.
-- * 'tagOptions' - Information about the TagOptions associated with the resource.
-- * 'usageInstructions' - Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
mkDescribeProvisioningParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProvisioningParametersResponse
mkDescribeProvisioningParametersResponse pResponseStatus_ =
  DescribeProvisioningParametersResponse'
    { provisioningArtifactPreferences =
        Lude.Nothing,
      provisioningArtifactParameters = Lude.Nothing,
      usageInstructions = Lude.Nothing,
      constraintSummaries = Lude.Nothing,
      tagOptions = Lude.Nothing,
      provisioningArtifactOutputs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about preferences, such as regions and accounts, for the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsProvisioningArtifactPreferences :: Lens.Lens' DescribeProvisioningParametersResponse (Lude.Maybe ProvisioningArtifactPreferences)
dpprsProvisioningArtifactPreferences = Lens.lens (provisioningArtifactPreferences :: DescribeProvisioningParametersResponse -> Lude.Maybe ProvisioningArtifactPreferences) (\s a -> s {provisioningArtifactPreferences = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsProvisioningArtifactPreferences "Use generic-lens or generic-optics with 'provisioningArtifactPreferences' instead." #-}

-- | Information about the parameters used to provision the product.
--
-- /Note:/ Consider using 'provisioningArtifactParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsProvisioningArtifactParameters :: Lens.Lens' DescribeProvisioningParametersResponse (Lude.Maybe [ProvisioningArtifactParameter])
dpprsProvisioningArtifactParameters = Lens.lens (provisioningArtifactParameters :: DescribeProvisioningParametersResponse -> Lude.Maybe [ProvisioningArtifactParameter]) (\s a -> s {provisioningArtifactParameters = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsProvisioningArtifactParameters "Use generic-lens or generic-optics with 'provisioningArtifactParameters' instead." #-}

-- | Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
--
-- /Note:/ Consider using 'usageInstructions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsUsageInstructions :: Lens.Lens' DescribeProvisioningParametersResponse (Lude.Maybe [UsageInstruction])
dpprsUsageInstructions = Lens.lens (usageInstructions :: DescribeProvisioningParametersResponse -> Lude.Maybe [UsageInstruction]) (\s a -> s {usageInstructions = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsUsageInstructions "Use generic-lens or generic-optics with 'usageInstructions' instead." #-}

-- | Information about the constraints used to provision the product.
--
-- /Note:/ Consider using 'constraintSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsConstraintSummaries :: Lens.Lens' DescribeProvisioningParametersResponse (Lude.Maybe [ConstraintSummary])
dpprsConstraintSummaries = Lens.lens (constraintSummaries :: DescribeProvisioningParametersResponse -> Lude.Maybe [ConstraintSummary]) (\s a -> s {constraintSummaries = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsConstraintSummaries "Use generic-lens or generic-optics with 'constraintSummaries' instead." #-}

-- | Information about the TagOptions associated with the resource.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsTagOptions :: Lens.Lens' DescribeProvisioningParametersResponse (Lude.Maybe [TagOptionSummary])
dpprsTagOptions = Lens.lens (tagOptions :: DescribeProvisioningParametersResponse -> Lude.Maybe [TagOptionSummary]) (\s a -> s {tagOptions = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsTagOptions "Use generic-lens or generic-optics with 'tagOptions' instead." #-}

-- | The output of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsProvisioningArtifactOutputs :: Lens.Lens' DescribeProvisioningParametersResponse (Lude.Maybe [ProvisioningArtifactOutput])
dpprsProvisioningArtifactOutputs = Lens.lens (provisioningArtifactOutputs :: DescribeProvisioningParametersResponse -> Lude.Maybe [ProvisioningArtifactOutput]) (\s a -> s {provisioningArtifactOutputs = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsProvisioningArtifactOutputs "Use generic-lens or generic-optics with 'provisioningArtifactOutputs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsResponseStatus :: Lens.Lens' DescribeProvisioningParametersResponse Lude.Int
dpprsResponseStatus = Lens.lens (responseStatus :: DescribeProvisioningParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProvisioningParametersResponse)
{-# DEPRECATED dpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
