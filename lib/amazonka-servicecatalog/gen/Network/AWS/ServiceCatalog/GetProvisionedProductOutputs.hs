{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API takes either a @ProvisonedProductId@ or a @ProvisionedProductName@ , along with a list of one or more output keys, and responds with the key/value pairs of those outputs.
module Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
  ( -- * Creating a request
    GetProvisionedProductOutputs (..),
    mkGetProvisionedProductOutputs,

    -- ** Request lenses
    gppoProvisionedProductName,
    gppoOutputKeys,
    gppoAcceptLanguage,
    gppoPageToken,
    gppoPageSize,
    gppoProvisionedProductId,

    -- * Destructuring the response
    GetProvisionedProductOutputsResponse (..),
    mkGetProvisionedProductOutputsResponse,

    -- ** Response lenses
    gpporsNextPageToken,
    gpporsOutputs,
    gpporsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkGetProvisionedProductOutputs' smart constructor.
data GetProvisionedProductOutputs = GetProvisionedProductOutputs'
  { -- | The name of the provisioned product that you want the outputs from.
    provisionedProductName :: Lude.Maybe Lude.Text,
    -- | The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
    outputKeys :: Lude.Maybe [Lude.Text],
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
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The identifier of the provisioned product that you want the outputs from.
    provisionedProductId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProvisionedProductOutputs' with the minimum fields required to make a request.
--
-- * 'provisionedProductName' - The name of the provisioned product that you want the outputs from.
-- * 'outputKeys' - The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
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
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'provisionedProductId' - The identifier of the provisioned product that you want the outputs from.
mkGetProvisionedProductOutputs ::
  GetProvisionedProductOutputs
mkGetProvisionedProductOutputs =
  GetProvisionedProductOutputs'
    { provisionedProductName =
        Lude.Nothing,
      outputKeys = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      provisionedProductId = Lude.Nothing
    }

-- | The name of the provisioned product that you want the outputs from.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoProvisionedProductName :: Lens.Lens' GetProvisionedProductOutputs (Lude.Maybe Lude.Text)
gppoProvisionedProductName = Lens.lens (provisionedProductName :: GetProvisionedProductOutputs -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductName = a} :: GetProvisionedProductOutputs)
{-# DEPRECATED gppoProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
--
-- /Note:/ Consider using 'outputKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoOutputKeys :: Lens.Lens' GetProvisionedProductOutputs (Lude.Maybe [Lude.Text])
gppoOutputKeys = Lens.lens (outputKeys :: GetProvisionedProductOutputs -> Lude.Maybe [Lude.Text]) (\s a -> s {outputKeys = a} :: GetProvisionedProductOutputs)
{-# DEPRECATED gppoOutputKeys "Use generic-lens or generic-optics with 'outputKeys' instead." #-}

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
gppoAcceptLanguage :: Lens.Lens' GetProvisionedProductOutputs (Lude.Maybe Lude.Text)
gppoAcceptLanguage = Lens.lens (acceptLanguage :: GetProvisionedProductOutputs -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: GetProvisionedProductOutputs)
{-# DEPRECATED gppoAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoPageToken :: Lens.Lens' GetProvisionedProductOutputs (Lude.Maybe Lude.Text)
gppoPageToken = Lens.lens (pageToken :: GetProvisionedProductOutputs -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetProvisionedProductOutputs)
{-# DEPRECATED gppoPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoPageSize :: Lens.Lens' GetProvisionedProductOutputs (Lude.Maybe Lude.Natural)
gppoPageSize = Lens.lens (pageSize :: GetProvisionedProductOutputs -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: GetProvisionedProductOutputs)
{-# DEPRECATED gppoPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The identifier of the provisioned product that you want the outputs from.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppoProvisionedProductId :: Lens.Lens' GetProvisionedProductOutputs (Lude.Maybe Lude.Text)
gppoProvisionedProductId = Lens.lens (provisionedProductId :: GetProvisionedProductOutputs -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductId = a} :: GetProvisionedProductOutputs)
{-# DEPRECATED gppoProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

instance Lude.AWSRequest GetProvisionedProductOutputs where
  type
    Rs GetProvisionedProductOutputs =
      GetProvisionedProductOutputsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProvisionedProductOutputsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Outputs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProvisionedProductOutputs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.GetProvisionedProductOutputs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetProvisionedProductOutputs where
  toJSON GetProvisionedProductOutputs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedProductName" Lude..=)
              Lude.<$> provisionedProductName,
            ("OutputKeys" Lude..=) Lude.<$> outputKeys,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            ("ProvisionedProductId" Lude..=) Lude.<$> provisionedProductId
          ]
      )

instance Lude.ToPath GetProvisionedProductOutputs where
  toPath = Lude.const "/"

instance Lude.ToQuery GetProvisionedProductOutputs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetProvisionedProductOutputsResponse' smart constructor.
data GetProvisionedProductOutputsResponse = GetProvisionedProductOutputsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
    outputs :: Lude.Maybe [RecordOutput],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProvisionedProductOutputsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'outputs' - Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
-- * 'responseStatus' - The response status code.
mkGetProvisionedProductOutputsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProvisionedProductOutputsResponse
mkGetProvisionedProductOutputsResponse pResponseStatus_ =
  GetProvisionedProductOutputsResponse'
    { nextPageToken =
        Lude.Nothing,
      outputs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporsNextPageToken :: Lens.Lens' GetProvisionedProductOutputsResponse (Lude.Maybe Lude.Text)
gpporsNextPageToken = Lens.lens (nextPageToken :: GetProvisionedProductOutputsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetProvisionedProductOutputsResponse)
{-# DEPRECATED gpporsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporsOutputs :: Lens.Lens' GetProvisionedProductOutputsResponse (Lude.Maybe [RecordOutput])
gpporsOutputs = Lens.lens (outputs :: GetProvisionedProductOutputsResponse -> Lude.Maybe [RecordOutput]) (\s a -> s {outputs = a} :: GetProvisionedProductOutputsResponse)
{-# DEPRECATED gpporsOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpporsResponseStatus :: Lens.Lens' GetProvisionedProductOutputsResponse Lude.Int
gpporsResponseStatus = Lens.lens (responseStatus :: GetProvisionedProductOutputsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProvisionedProductOutputsResponse)
{-# DEPRECATED gpporsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
