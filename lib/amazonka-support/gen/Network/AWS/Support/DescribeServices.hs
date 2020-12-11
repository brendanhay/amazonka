{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current list of AWS services and a list of service categories for each service. You then use service names and categories in your 'CreateCase' requests. Each AWS service has its own set of categories.
--
-- The service codes and category codes correspond to the values that appear in the __Service__ and __Category__ lists on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page. The values in those fields don't necessarily match the service codes and categories returned by the @DescribeServices@ operation. Always use the service codes and categories that the @DescribeServices@ operation returns, so that you have the most recent set of service and category codes.
module Network.AWS.Support.DescribeServices
  ( -- * Creating a request
    DescribeServices (..),
    mkDescribeServices,

    -- ** Request lenses
    dsServiceCodeList,
    dsLanguage,

    -- * Destructuring the response
    DescribeServicesResponse (..),
    mkDescribeServicesResponse,

    -- ** Response lenses
    dsrsServices,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { serviceCodeList ::
      Lude.Maybe [Lude.Text],
    language :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServices' with the minimum fields required to make a request.
--
-- * 'language' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
-- * 'serviceCodeList' - A JSON-formatted list of service codes available for AWS services.
mkDescribeServices ::
  DescribeServices
mkDescribeServices =
  DescribeServices'
    { serviceCodeList = Lude.Nothing,
      language = Lude.Nothing
    }

-- | A JSON-formatted list of service codes available for AWS services.
--
-- /Note:/ Consider using 'serviceCodeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceCodeList :: Lens.Lens' DescribeServices (Lude.Maybe [Lude.Text])
dsServiceCodeList = Lens.lens (serviceCodeList :: DescribeServices -> Lude.Maybe [Lude.Text]) (\s a -> s {serviceCodeList = a} :: DescribeServices)
{-# DEPRECATED dsServiceCodeList "Use generic-lens or generic-optics with 'serviceCodeList' instead." #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLanguage :: Lens.Lens' DescribeServices (Lude.Maybe Lude.Text)
dsLanguage = Lens.lens (language :: DescribeServices -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: DescribeServices)
{-# DEPRECATED dsLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Lude.AWSRequest DescribeServices where
  type Rs DescribeServices = DescribeServicesResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Lude.<$> (x Lude..?> "services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.DescribeServices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceCodeList" Lude..=) Lude.<$> serviceCodeList,
            ("language" Lude..=) Lude.<$> language
          ]
      )

instance Lude.ToPath DescribeServices where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServices where
  toQuery = Lude.const Lude.mempty

-- | The list of AWS services returned by the 'DescribeServices' operation.
--
-- /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { services ::
      Lude.Maybe [SupportService],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServicesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'services' - A JSON-formatted list of AWS services.
mkDescribeServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServicesResponse
mkDescribeServicesResponse pResponseStatus_ =
  DescribeServicesResponse'
    { services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JSON-formatted list of AWS services.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsServices :: Lens.Lens' DescribeServicesResponse (Lude.Maybe [SupportService])
dsrsServices = Lens.lens (services :: DescribeServicesResponse -> Lude.Maybe [SupportService]) (\s a -> s {services = a} :: DescribeServicesResponse)
{-# DEPRECATED dsrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeServicesResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServicesResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
