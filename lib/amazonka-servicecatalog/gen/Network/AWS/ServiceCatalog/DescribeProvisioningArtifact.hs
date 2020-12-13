{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioning artifact (also known as a version) for the specified product.
module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
  ( -- * Creating a request
    DescribeProvisioningArtifact (..),
    mkDescribeProvisioningArtifact,

    -- ** Request lenses
    dpaProductName,
    dpaProvisioningArtifactId,
    dpaVerbose,
    dpaProvisioningArtifactName,
    dpaAcceptLanguage,
    dpaProductId,

    -- * Destructuring the response
    DescribeProvisioningArtifactResponse (..),
    mkDescribeProvisioningArtifactResponse,

    -- ** Response lenses
    dparsStatus,
    dparsInfo,
    dparsProvisioningArtifactDetail,
    dparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
  { -- | The product name.
    productName :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Lude.Maybe Lude.Text,
    -- | Indicates whether a verbose level of detail is enabled.
    verbose :: Lude.Maybe Lude.Bool,
    -- | The provisioning artifact name.
    provisioningArtifactName :: Lude.Maybe Lude.Text,
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
    -- | The product identifier.
    productId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningArtifact' with the minimum fields required to make a request.
--
-- * 'productName' - The product name.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
-- * 'verbose' - Indicates whether a verbose level of detail is enabled.
-- * 'provisioningArtifactName' - The provisioning artifact name.
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
-- * 'productId' - The product identifier.
mkDescribeProvisioningArtifact ::
  DescribeProvisioningArtifact
mkDescribeProvisioningArtifact =
  DescribeProvisioningArtifact'
    { productName = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      verbose = Lude.Nothing,
      provisioningArtifactName = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      productId = Lude.Nothing
    }

-- | The product name.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProductName :: Lens.Lens' DescribeProvisioningArtifact (Lude.Maybe Lude.Text)
dpaProductName = Lens.lens (productName :: DescribeProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {productName = a} :: DescribeProvisioningArtifact)
{-# DEPRECATED dpaProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProvisioningArtifactId :: Lens.Lens' DescribeProvisioningArtifact (Lude.Maybe Lude.Text)
dpaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: DescribeProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: DescribeProvisioningArtifact)
{-# DEPRECATED dpaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | Indicates whether a verbose level of detail is enabled.
--
-- /Note:/ Consider using 'verbose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaVerbose :: Lens.Lens' DescribeProvisioningArtifact (Lude.Maybe Lude.Bool)
dpaVerbose = Lens.lens (verbose :: DescribeProvisioningArtifact -> Lude.Maybe Lude.Bool) (\s a -> s {verbose = a} :: DescribeProvisioningArtifact)
{-# DEPRECATED dpaVerbose "Use generic-lens or generic-optics with 'verbose' instead." #-}

-- | The provisioning artifact name.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProvisioningArtifactName :: Lens.Lens' DescribeProvisioningArtifact (Lude.Maybe Lude.Text)
dpaProvisioningArtifactName = Lens.lens (provisioningArtifactName :: DescribeProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactName = a} :: DescribeProvisioningArtifact)
{-# DEPRECATED dpaProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

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
dpaAcceptLanguage :: Lens.Lens' DescribeProvisioningArtifact (Lude.Maybe Lude.Text)
dpaAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProvisioningArtifact)
{-# DEPRECATED dpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProductId :: Lens.Lens' DescribeProvisioningArtifact (Lude.Maybe Lude.Text)
dpaProductId = Lens.lens (productId :: DescribeProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: DescribeProvisioningArtifact)
{-# DEPRECATED dpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.AWSRequest DescribeProvisioningArtifact where
  type
    Rs DescribeProvisioningArtifact =
      DescribeProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProvisioningArtifactResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Info" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ProvisioningArtifactDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProvisioningArtifact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProvisioningArtifact where
  toJSON DescribeProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProductName" Lude..=) Lude.<$> productName,
            ("ProvisioningArtifactId" Lude..=) Lude.<$> provisioningArtifactId,
            ("Verbose" Lude..=) Lude.<$> verbose,
            ("ProvisioningArtifactName" Lude..=)
              Lude.<$> provisioningArtifactName,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("ProductId" Lude..=) Lude.<$> productId
          ]
      )

instance Lude.ToPath DescribeProvisioningArtifact where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProvisioningArtifact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
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

-- | Creates a value of 'DescribeProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the current request.
-- * 'info' - The URL of the CloudFormation template in Amazon S3.
-- * 'provisioningArtifactDetail' - Information about the provisioning artifact.
-- * 'responseStatus' - The response status code.
mkDescribeProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProvisioningArtifactResponse
mkDescribeProvisioningArtifactResponse pResponseStatus_ =
  DescribeProvisioningArtifactResponse'
    { status = Lude.Nothing,
      info = Lude.Nothing,
      provisioningArtifactDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparsStatus :: Lens.Lens' DescribeProvisioningArtifactResponse (Lude.Maybe RequestStatus)
dparsStatus = Lens.lens (status :: DescribeProvisioningArtifactResponse -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: DescribeProvisioningArtifactResponse)
{-# DEPRECATED dparsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The URL of the CloudFormation template in Amazon S3.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparsInfo :: Lens.Lens' DescribeProvisioningArtifactResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dparsInfo = Lens.lens (info :: DescribeProvisioningArtifactResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {info = a} :: DescribeProvisioningArtifactResponse)
{-# DEPRECATED dparsInfo "Use generic-lens or generic-optics with 'info' instead." #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparsProvisioningArtifactDetail :: Lens.Lens' DescribeProvisioningArtifactResponse (Lude.Maybe ProvisioningArtifactDetail)
dparsProvisioningArtifactDetail = Lens.lens (provisioningArtifactDetail :: DescribeProvisioningArtifactResponse -> Lude.Maybe ProvisioningArtifactDetail) (\s a -> s {provisioningArtifactDetail = a} :: DescribeProvisioningArtifactResponse)
{-# DEPRECATED dparsProvisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparsResponseStatus :: Lens.Lens' DescribeProvisioningArtifactResponse Lude.Int
dparsResponseStatus = Lens.lens (responseStatus :: DescribeProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProvisioningArtifactResponse)
{-# DEPRECATED dparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
