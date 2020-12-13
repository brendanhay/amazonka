{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified request operation.
--
-- Use this operation after calling a request operation (for example, 'ProvisionProduct' , 'TerminateProvisionedProduct' , or 'UpdateProvisionedProduct' ).
module Network.AWS.ServiceCatalog.DescribeRecord
  ( -- * Creating a request
    DescribeRecord (..),
    mkDescribeRecord,

    -- ** Request lenses
    drAcceptLanguage,
    drId,
    drPageToken,
    drPageSize,

    -- * Destructuring the response
    DescribeRecordResponse (..),
    mkDescribeRecordResponse,

    -- ** Response lenses
    drrsRecordDetail,
    drrsNextPageToken,
    drrsRecordOutputs,
    drrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeRecord' smart constructor.
data DescribeRecord = DescribeRecord'
  { -- | The language code.
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
    -- | The record identifier of the provisioned product. This identifier is returned by the request operation.
    id :: Lude.Text,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRecord' with the minimum fields required to make a request.
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
-- * 'id' - The record identifier of the provisioned product. This identifier is returned by the request operation.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
mkDescribeRecord ::
  -- | 'id'
  Lude.Text ->
  DescribeRecord
mkDescribeRecord pId_ =
  DescribeRecord'
    { acceptLanguage = Lude.Nothing,
      id = pId_,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

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
drAcceptLanguage :: Lens.Lens' DescribeRecord (Lude.Maybe Lude.Text)
drAcceptLanguage = Lens.lens (acceptLanguage :: DescribeRecord -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeRecord)
{-# DEPRECATED drAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The record identifier of the provisioned product. This identifier is returned by the request operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drId :: Lens.Lens' DescribeRecord Lude.Text
drId = Lens.lens (id :: DescribeRecord -> Lude.Text) (\s a -> s {id = a} :: DescribeRecord)
{-# DEPRECATED drId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drPageToken :: Lens.Lens' DescribeRecord (Lude.Maybe Lude.Text)
drPageToken = Lens.lens (pageToken :: DescribeRecord -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: DescribeRecord)
{-# DEPRECATED drPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drPageSize :: Lens.Lens' DescribeRecord (Lude.Maybe Lude.Natural)
drPageSize = Lens.lens (pageSize :: DescribeRecord -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeRecord)
{-# DEPRECATED drPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest DescribeRecord where
  type Rs DescribeRecord = DescribeRecordResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRecordResponse'
            Lude.<$> (x Lude..?> "RecordDetail")
            Lude.<*> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "RecordOutputs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRecord where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.DescribeRecord" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRecord where
  toJSON DescribeRecord' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id),
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath DescribeRecord where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRecord where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRecordResponse' smart constructor.
data DescribeRecordResponse = DescribeRecordResponse'
  { -- | Information about the product.
    recordDetail :: Lude.Maybe RecordDetail,
    -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
    recordOutputs :: Lude.Maybe [RecordOutput],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRecordResponse' with the minimum fields required to make a request.
--
-- * 'recordDetail' - Information about the product.
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'recordOutputs' - Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
-- * 'responseStatus' - The response status code.
mkDescribeRecordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRecordResponse
mkDescribeRecordResponse pResponseStatus_ =
  DescribeRecordResponse'
    { recordDetail = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      recordOutputs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRecordDetail :: Lens.Lens' DescribeRecordResponse (Lude.Maybe RecordDetail)
drrsRecordDetail = Lens.lens (recordDetail :: DescribeRecordResponse -> Lude.Maybe RecordDetail) (\s a -> s {recordDetail = a} :: DescribeRecordResponse)
{-# DEPRECATED drrsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsNextPageToken :: Lens.Lens' DescribeRecordResponse (Lude.Maybe Lude.Text)
drrsNextPageToken = Lens.lens (nextPageToken :: DescribeRecordResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: DescribeRecordResponse)
{-# DEPRECATED drrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- /Note:/ Consider using 'recordOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRecordOutputs :: Lens.Lens' DescribeRecordResponse (Lude.Maybe [RecordOutput])
drrsRecordOutputs = Lens.lens (recordOutputs :: DescribeRecordResponse -> Lude.Maybe [RecordOutput]) (\s a -> s {recordOutputs = a} :: DescribeRecordResponse)
{-# DEPRECATED drrsRecordOutputs "Use generic-lens or generic-optics with 'recordOutputs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DescribeRecordResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DescribeRecordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRecordResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
