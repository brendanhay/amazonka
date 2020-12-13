{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds the default parameters for a specific self-service action on a specific provisioned product and returns a map of the results to the user.
module Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
  ( -- * Creating a request
    DescribeServiceActionExecutionParameters (..),
    mkDescribeServiceActionExecutionParameters,

    -- ** Request lenses
    dsaepAcceptLanguage,
    dsaepProvisionedProductId,
    dsaepServiceActionId,

    -- * Destructuring the response
    DescribeServiceActionExecutionParametersResponse (..),
    mkDescribeServiceActionExecutionParametersResponse,

    -- ** Response lenses
    dsaeprsServiceActionParameters,
    dsaeprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeServiceActionExecutionParameters' smart constructor.
data DescribeServiceActionExecutionParameters = DescribeServiceActionExecutionParameters'
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
    -- | The identifier of the provisioned product.
    provisionedProductId :: Lude.Text,
    -- | The self-service action identifier.
    serviceActionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceActionExecutionParameters' with the minimum fields required to make a request.
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
-- * 'provisionedProductId' - The identifier of the provisioned product.
-- * 'serviceActionId' - The self-service action identifier.
mkDescribeServiceActionExecutionParameters ::
  -- | 'provisionedProductId'
  Lude.Text ->
  -- | 'serviceActionId'
  Lude.Text ->
  DescribeServiceActionExecutionParameters
mkDescribeServiceActionExecutionParameters
  pProvisionedProductId_
  pServiceActionId_ =
    DescribeServiceActionExecutionParameters'
      { acceptLanguage =
          Lude.Nothing,
        provisionedProductId = pProvisionedProductId_,
        serviceActionId = pServiceActionId_
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
dsaepAcceptLanguage :: Lens.Lens' DescribeServiceActionExecutionParameters (Lude.Maybe Lude.Text)
dsaepAcceptLanguage = Lens.lens (acceptLanguage :: DescribeServiceActionExecutionParameters -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeServiceActionExecutionParameters)
{-# DEPRECATED dsaepAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaepProvisionedProductId :: Lens.Lens' DescribeServiceActionExecutionParameters Lude.Text
dsaepProvisionedProductId = Lens.lens (provisionedProductId :: DescribeServiceActionExecutionParameters -> Lude.Text) (\s a -> s {provisionedProductId = a} :: DescribeServiceActionExecutionParameters)
{-# DEPRECATED dsaepProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaepServiceActionId :: Lens.Lens' DescribeServiceActionExecutionParameters Lude.Text
dsaepServiceActionId = Lens.lens (serviceActionId :: DescribeServiceActionExecutionParameters -> Lude.Text) (\s a -> s {serviceActionId = a} :: DescribeServiceActionExecutionParameters)
{-# DEPRECATED dsaepServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

instance Lude.AWSRequest DescribeServiceActionExecutionParameters where
  type
    Rs DescribeServiceActionExecutionParameters =
      DescribeServiceActionExecutionParametersResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeServiceActionExecutionParametersResponse'
            Lude.<$> (x Lude..?> "ServiceActionParameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServiceActionExecutionParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeServiceActionExecutionParameters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeServiceActionExecutionParameters where
  toJSON DescribeServiceActionExecutionParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ProvisionedProductId" Lude..= provisionedProductId),
            Lude.Just ("ServiceActionId" Lude..= serviceActionId)
          ]
      )

instance Lude.ToPath DescribeServiceActionExecutionParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServiceActionExecutionParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeServiceActionExecutionParametersResponse' smart constructor.
data DescribeServiceActionExecutionParametersResponse = DescribeServiceActionExecutionParametersResponse'
  { -- | The parameters of the self-service action.
    serviceActionParameters :: Lude.Maybe [ExecutionParameter],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceActionExecutionParametersResponse' with the minimum fields required to make a request.
--
-- * 'serviceActionParameters' - The parameters of the self-service action.
-- * 'responseStatus' - The response status code.
mkDescribeServiceActionExecutionParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServiceActionExecutionParametersResponse
mkDescribeServiceActionExecutionParametersResponse pResponseStatus_ =
  DescribeServiceActionExecutionParametersResponse'
    { serviceActionParameters =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The parameters of the self-service action.
--
-- /Note:/ Consider using 'serviceActionParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaeprsServiceActionParameters :: Lens.Lens' DescribeServiceActionExecutionParametersResponse (Lude.Maybe [ExecutionParameter])
dsaeprsServiceActionParameters = Lens.lens (serviceActionParameters :: DescribeServiceActionExecutionParametersResponse -> Lude.Maybe [ExecutionParameter]) (\s a -> s {serviceActionParameters = a} :: DescribeServiceActionExecutionParametersResponse)
{-# DEPRECATED dsaeprsServiceActionParameters "Use generic-lens or generic-optics with 'serviceActionParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaeprsResponseStatus :: Lens.Lens' DescribeServiceActionExecutionParametersResponse Lude.Int
dsaeprsResponseStatus = Lens.lens (responseStatus :: DescribeServiceActionExecutionParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServiceActionExecutionParametersResponse)
{-# DEPRECATED dsaeprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
