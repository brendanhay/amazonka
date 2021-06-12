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
-- Module      : Network.AWS.Support.DescribeServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current list of AWS services and a list of service
-- categories for each service. You then use service names and categories
-- in your CreateCase requests. Each AWS service has its own set of
-- categories.
--
-- The service codes and category codes correspond to the values that
-- appear in the __Service__ and __Category__ lists on the AWS Support
-- Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page. The values in those fields don\'t necessarily match the service
-- codes and categories returned by the @DescribeServices@ operation.
-- Always use the service codes and categories that the @DescribeServices@
-- operation returns, so that you have the most recent set of service and
-- category codes.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.DescribeServices
  ( -- * Creating a Request
    DescribeServices (..),
    newDescribeServices,

    -- * Request Lenses
    describeServices_serviceCodeList,
    describeServices_language,

    -- * Destructuring the Response
    DescribeServicesResponse (..),
    newDescribeServicesResponse,

    -- * Response Lenses
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | A JSON-formatted list of service codes available for AWS services.
    serviceCodeList :: Core.Maybe [Core.Text],
    -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCodeList', 'describeServices_serviceCodeList' - A JSON-formatted list of service codes available for AWS services.
--
-- 'language', 'describeServices_language' - The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
newDescribeServices ::
  DescribeServices
newDescribeServices =
  DescribeServices'
    { serviceCodeList = Core.Nothing,
      language = Core.Nothing
    }

-- | A JSON-formatted list of service codes available for AWS services.
describeServices_serviceCodeList :: Lens.Lens' DescribeServices (Core.Maybe [Core.Text])
describeServices_serviceCodeList = Lens.lens (\DescribeServices' {serviceCodeList} -> serviceCodeList) (\s@DescribeServices' {} a -> s {serviceCodeList = a} :: DescribeServices) Core.. Lens.mapping Lens._Coerce

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
describeServices_language :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
describeServices_language = Lens.lens (\DescribeServices' {language} -> language) (\s@DescribeServices' {} a -> s {language = a} :: DescribeServices)

instance Core.AWSRequest DescribeServices where
  type
    AWSResponse DescribeServices =
      DescribeServicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Core.<$> (x Core..?> "services" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeServices

instance Core.NFData DescribeServices

instance Core.ToHeaders DescribeServices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeServices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serviceCodeList" Core..=)
              Core.<$> serviceCodeList,
            ("language" Core..=) Core.<$> language
          ]
      )

instance Core.ToPath DescribeServices where
  toPath = Core.const "/"

instance Core.ToQuery DescribeServices where
  toQuery = Core.const Core.mempty

-- | The list of AWS services returned by the DescribeServices operation.
--
-- /See:/ 'newDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | A JSON-formatted list of AWS services.
    services :: Core.Maybe [SupportService],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'describeServicesResponse_services' - A JSON-formatted list of AWS services.
--
-- 'httpStatus', 'describeServicesResponse_httpStatus' - The response's http status code.
newDescribeServicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeServicesResponse
newDescribeServicesResponse pHttpStatus_ =
  DescribeServicesResponse'
    { services = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON-formatted list of AWS services.
describeServicesResponse_services :: Lens.Lens' DescribeServicesResponse (Core.Maybe [SupportService])
describeServicesResponse_services = Lens.lens (\DescribeServicesResponse' {services} -> services) (\s@DescribeServicesResponse' {} a -> s {services = a} :: DescribeServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeServicesResponse_httpStatus :: Lens.Lens' DescribeServicesResponse Core.Int
describeServicesResponse_httpStatus = Lens.lens (\DescribeServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeServicesResponse' {} a -> s {httpStatus = a} :: DescribeServicesResponse)

instance Core.NFData DescribeServicesResponse
