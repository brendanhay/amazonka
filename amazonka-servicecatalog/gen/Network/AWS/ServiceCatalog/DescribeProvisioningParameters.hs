{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the configuration required to provision the
-- specified product using the specified provisioning artifact.
--
-- If the output contains a TagOption key with an empty list of values,
-- there is a TagOption conflict for that key. The end user cannot take
-- action to fix the conflict, and launch is not blocked. In subsequent
-- calls to ProvisionProduct, do not include conflicted TagOption keys as
-- tags, or this causes the error \"Parameter validation failed: Missing
-- required parameter in Tags[/N/]:/Value/\". Tag the provisioned product
-- with the value @sc-tagoption-conflict-portfolioId-productId@.
module Network.AWS.ServiceCatalog.DescribeProvisioningParameters
  ( -- * Creating a Request
    DescribeProvisioningParameters (..),
    newDescribeProvisioningParameters,

    -- * Request Lenses
    describeProvisioningParameters_provisioningArtifactName,
    describeProvisioningParameters_provisioningArtifactId,
    describeProvisioningParameters_productName,
    describeProvisioningParameters_productId,
    describeProvisioningParameters_pathId,
    describeProvisioningParameters_acceptLanguage,
    describeProvisioningParameters_pathName,

    -- * Destructuring the Response
    DescribeProvisioningParametersResponse (..),
    newDescribeProvisioningParametersResponse,

    -- * Response Lenses
    describeProvisioningParametersResponse_constraintSummaries,
    describeProvisioningParametersResponse_usageInstructions,
    describeProvisioningParametersResponse_provisioningArtifactOutputs,
    describeProvisioningParametersResponse_provisioningArtifactPreferences,
    describeProvisioningParametersResponse_provisioningArtifactParameters,
    describeProvisioningParametersResponse_tagOptions,
    describeProvisioningParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProvisioningParameters' smart constructor.
data DescribeProvisioningParameters = DescribeProvisioningParameters'
  { -- | The name of the provisioning artifact. You must provide the name or ID,
    -- but not both.
    provisioningArtifactName :: Core.Maybe Core.Text,
    -- | The identifier of the provisioning artifact. You must provide the name
    -- or ID, but not both.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The name of the product. You must provide the name or ID, but not both.
    productName :: Core.Maybe Core.Text,
    -- | The product identifier. You must provide the product name or ID, but not
    -- both.
    productId :: Core.Maybe Core.Text,
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path. To list the paths for a product, use ListLaunchPaths. You must
    -- provide the name or ID, but not both.
    pathId :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The name of the path. You must provide the name or ID, but not both.
    pathName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProvisioningParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifactName', 'describeProvisioningParameters_provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
--
-- 'provisioningArtifactId', 'describeProvisioningParameters_provisioningArtifactId' - The identifier of the provisioning artifact. You must provide the name
-- or ID, but not both.
--
-- 'productName', 'describeProvisioningParameters_productName' - The name of the product. You must provide the name or ID, but not both.
--
-- 'productId', 'describeProvisioningParameters_productId' - The product identifier. You must provide the product name or ID, but not
-- both.
--
-- 'pathId', 'describeProvisioningParameters_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths. You must
-- provide the name or ID, but not both.
--
-- 'acceptLanguage', 'describeProvisioningParameters_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pathName', 'describeProvisioningParameters_pathName' - The name of the path. You must provide the name or ID, but not both.
newDescribeProvisioningParameters ::
  DescribeProvisioningParameters
newDescribeProvisioningParameters =
  DescribeProvisioningParameters'
    { provisioningArtifactName =
        Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      productName = Core.Nothing,
      productId = Core.Nothing,
      pathId = Core.Nothing,
      acceptLanguage = Core.Nothing,
      pathName = Core.Nothing
    }

-- | The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
describeProvisioningParameters_provisioningArtifactName :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_provisioningArtifactName = Lens.lens (\DescribeProvisioningParameters' {provisioningArtifactName} -> provisioningArtifactName) (\s@DescribeProvisioningParameters' {} a -> s {provisioningArtifactName = a} :: DescribeProvisioningParameters)

-- | The identifier of the provisioning artifact. You must provide the name
-- or ID, but not both.
describeProvisioningParameters_provisioningArtifactId :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_provisioningArtifactId = Lens.lens (\DescribeProvisioningParameters' {provisioningArtifactId} -> provisioningArtifactId) (\s@DescribeProvisioningParameters' {} a -> s {provisioningArtifactId = a} :: DescribeProvisioningParameters)

-- | The name of the product. You must provide the name or ID, but not both.
describeProvisioningParameters_productName :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_productName = Lens.lens (\DescribeProvisioningParameters' {productName} -> productName) (\s@DescribeProvisioningParameters' {} a -> s {productName = a} :: DescribeProvisioningParameters)

-- | The product identifier. You must provide the product name or ID, but not
-- both.
describeProvisioningParameters_productId :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_productId = Lens.lens (\DescribeProvisioningParameters' {productId} -> productId) (\s@DescribeProvisioningParameters' {} a -> s {productId = a} :: DescribeProvisioningParameters)

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths. You must
-- provide the name or ID, but not both.
describeProvisioningParameters_pathId :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_pathId = Lens.lens (\DescribeProvisioningParameters' {pathId} -> pathId) (\s@DescribeProvisioningParameters' {} a -> s {pathId = a} :: DescribeProvisioningParameters)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisioningParameters_acceptLanguage :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_acceptLanguage = Lens.lens (\DescribeProvisioningParameters' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisioningParameters' {} a -> s {acceptLanguage = a} :: DescribeProvisioningParameters)

-- | The name of the path. You must provide the name or ID, but not both.
describeProvisioningParameters_pathName :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Core.Text)
describeProvisioningParameters_pathName = Lens.lens (\DescribeProvisioningParameters' {pathName} -> pathName) (\s@DescribeProvisioningParameters' {} a -> s {pathName = a} :: DescribeProvisioningParameters)

instance
  Core.AWSRequest
    DescribeProvisioningParameters
  where
  type
    AWSResponse DescribeProvisioningParameters =
      DescribeProvisioningParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningParametersResponse'
            Core.<$> ( x Core..?> "ConstraintSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "UsageInstructions" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "ProvisioningArtifactOutputs"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "ProvisioningArtifactPreferences")
            Core.<*> ( x Core..?> "ProvisioningArtifactParameters"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "TagOptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProvisioningParameters

instance Core.NFData DescribeProvisioningParameters

instance
  Core.ToHeaders
    DescribeProvisioningParameters
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProvisioningParameters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProvisioningParameters where
  toJSON DescribeProvisioningParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisioningArtifactName" Core..=)
              Core.<$> provisioningArtifactName,
            ("ProvisioningArtifactId" Core..=)
              Core.<$> provisioningArtifactId,
            ("ProductName" Core..=) Core.<$> productName,
            ("ProductId" Core..=) Core.<$> productId,
            ("PathId" Core..=) Core.<$> pathId,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PathName" Core..=) Core.<$> pathName
          ]
      )

instance Core.ToPath DescribeProvisioningParameters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProvisioningParameters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProvisioningParametersResponse' smart constructor.
data DescribeProvisioningParametersResponse = DescribeProvisioningParametersResponse'
  { -- | Information about the constraints used to provision the product.
    constraintSummaries :: Core.Maybe [ConstraintSummary],
    -- | Any additional metadata specifically related to the provisioning of the
    -- product. For example, see the @Version@ field of the CloudFormation
    -- template.
    usageInstructions :: Core.Maybe [UsageInstruction],
    -- | The output of the provisioning artifact.
    provisioningArtifactOutputs :: Core.Maybe [ProvisioningArtifactOutput],
    -- | An object that contains information about preferences, such as regions
    -- and accounts, for the provisioning artifact.
    provisioningArtifactPreferences :: Core.Maybe ProvisioningArtifactPreferences,
    -- | Information about the parameters used to provision the product.
    provisioningArtifactParameters :: Core.Maybe [ProvisioningArtifactParameter],
    -- | Information about the TagOptions associated with the resource.
    tagOptions :: Core.Maybe [TagOptionSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProvisioningParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintSummaries', 'describeProvisioningParametersResponse_constraintSummaries' - Information about the constraints used to provision the product.
--
-- 'usageInstructions', 'describeProvisioningParametersResponse_usageInstructions' - Any additional metadata specifically related to the provisioning of the
-- product. For example, see the @Version@ field of the CloudFormation
-- template.
--
-- 'provisioningArtifactOutputs', 'describeProvisioningParametersResponse_provisioningArtifactOutputs' - The output of the provisioning artifact.
--
-- 'provisioningArtifactPreferences', 'describeProvisioningParametersResponse_provisioningArtifactPreferences' - An object that contains information about preferences, such as regions
-- and accounts, for the provisioning artifact.
--
-- 'provisioningArtifactParameters', 'describeProvisioningParametersResponse_provisioningArtifactParameters' - Information about the parameters used to provision the product.
--
-- 'tagOptions', 'describeProvisioningParametersResponse_tagOptions' - Information about the TagOptions associated with the resource.
--
-- 'httpStatus', 'describeProvisioningParametersResponse_httpStatus' - The response's http status code.
newDescribeProvisioningParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProvisioningParametersResponse
newDescribeProvisioningParametersResponse
  pHttpStatus_ =
    DescribeProvisioningParametersResponse'
      { constraintSummaries =
          Core.Nothing,
        usageInstructions = Core.Nothing,
        provisioningArtifactOutputs =
          Core.Nothing,
        provisioningArtifactPreferences =
          Core.Nothing,
        provisioningArtifactParameters =
          Core.Nothing,
        tagOptions = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the constraints used to provision the product.
describeProvisioningParametersResponse_constraintSummaries :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [ConstraintSummary])
describeProvisioningParametersResponse_constraintSummaries = Lens.lens (\DescribeProvisioningParametersResponse' {constraintSummaries} -> constraintSummaries) (\s@DescribeProvisioningParametersResponse' {} a -> s {constraintSummaries = a} :: DescribeProvisioningParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | Any additional metadata specifically related to the provisioning of the
-- product. For example, see the @Version@ field of the CloudFormation
-- template.
describeProvisioningParametersResponse_usageInstructions :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [UsageInstruction])
describeProvisioningParametersResponse_usageInstructions = Lens.lens (\DescribeProvisioningParametersResponse' {usageInstructions} -> usageInstructions) (\s@DescribeProvisioningParametersResponse' {} a -> s {usageInstructions = a} :: DescribeProvisioningParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The output of the provisioning artifact.
describeProvisioningParametersResponse_provisioningArtifactOutputs :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [ProvisioningArtifactOutput])
describeProvisioningParametersResponse_provisioningArtifactOutputs = Lens.lens (\DescribeProvisioningParametersResponse' {provisioningArtifactOutputs} -> provisioningArtifactOutputs) (\s@DescribeProvisioningParametersResponse' {} a -> s {provisioningArtifactOutputs = a} :: DescribeProvisioningParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | An object that contains information about preferences, such as regions
-- and accounts, for the provisioning artifact.
describeProvisioningParametersResponse_provisioningArtifactPreferences :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe ProvisioningArtifactPreferences)
describeProvisioningParametersResponse_provisioningArtifactPreferences = Lens.lens (\DescribeProvisioningParametersResponse' {provisioningArtifactPreferences} -> provisioningArtifactPreferences) (\s@DescribeProvisioningParametersResponse' {} a -> s {provisioningArtifactPreferences = a} :: DescribeProvisioningParametersResponse)

-- | Information about the parameters used to provision the product.
describeProvisioningParametersResponse_provisioningArtifactParameters :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [ProvisioningArtifactParameter])
describeProvisioningParametersResponse_provisioningArtifactParameters = Lens.lens (\DescribeProvisioningParametersResponse' {provisioningArtifactParameters} -> provisioningArtifactParameters) (\s@DescribeProvisioningParametersResponse' {} a -> s {provisioningArtifactParameters = a} :: DescribeProvisioningParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the TagOptions associated with the resource.
describeProvisioningParametersResponse_tagOptions :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [TagOptionSummary])
describeProvisioningParametersResponse_tagOptions = Lens.lens (\DescribeProvisioningParametersResponse' {tagOptions} -> tagOptions) (\s@DescribeProvisioningParametersResponse' {} a -> s {tagOptions = a} :: DescribeProvisioningParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProvisioningParametersResponse_httpStatus :: Lens.Lens' DescribeProvisioningParametersResponse Core.Int
describeProvisioningParametersResponse_httpStatus = Lens.lens (\DescribeProvisioningParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningParametersResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningParametersResponse)

instance
  Core.NFData
    DescribeProvisioningParametersResponse
