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
-- Module      : Amazonka.Support.DescribeServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current list of Amazon Web Services services and a list of
-- service categories for each service. You then use service names and
-- categories in your CreateCase requests. Each Amazon Web Services service
-- has its own set of categories.
--
-- The service codes and category codes correspond to the values that
-- appear in the __Service__ and __Category__ lists on the Amazon Web
-- Services Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page. The values in those fields don\'t necessarily match the service
-- codes and categories returned by the @DescribeServices@ operation.
-- Always use the service codes and categories that the @DescribeServices@
-- operation returns, so that you have the most recent set of service and
-- category codes.
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     doesn\'t have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
module Amazonka.Support.DescribeServices
  ( -- * Creating a Request
    DescribeServices (..),
    newDescribeServices,

    -- * Request Lenses
    describeServices_language,
    describeServices_serviceCodeList,

    -- * Destructuring the Response
    DescribeServicesResponse (..),
    newDescribeServicesResponse,

    -- * Response Lenses
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Support.Types

-- | /See:/ 'newDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | The language in which Amazon Web Services Support handles the case.
    -- Amazon Web Services Support currently supports Chinese (“zh”), English
    -- (\"en\"), Japanese (\"ja\") and Korean (“ko”). You must specify the ISO
    -- 639-1 code for the @language@ parameter if you want support in that
    -- language.
    language :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted list of service codes available for Amazon Web Services
    -- services.
    serviceCodeList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'describeServices_language' - The language in which Amazon Web Services Support handles the case.
-- Amazon Web Services Support currently supports Chinese (“zh”), English
-- (\"en\"), Japanese (\"ja\") and Korean (“ko”). You must specify the ISO
-- 639-1 code for the @language@ parameter if you want support in that
-- language.
--
-- 'serviceCodeList', 'describeServices_serviceCodeList' - A JSON-formatted list of service codes available for Amazon Web Services
-- services.
newDescribeServices ::
  DescribeServices
newDescribeServices =
  DescribeServices'
    { language = Prelude.Nothing,
      serviceCodeList = Prelude.Nothing
    }

-- | The language in which Amazon Web Services Support handles the case.
-- Amazon Web Services Support currently supports Chinese (“zh”), English
-- (\"en\"), Japanese (\"ja\") and Korean (“ko”). You must specify the ISO
-- 639-1 code for the @language@ parameter if you want support in that
-- language.
describeServices_language :: Lens.Lens' DescribeServices (Prelude.Maybe Prelude.Text)
describeServices_language = Lens.lens (\DescribeServices' {language} -> language) (\s@DescribeServices' {} a -> s {language = a} :: DescribeServices)

-- | A JSON-formatted list of service codes available for Amazon Web Services
-- services.
describeServices_serviceCodeList :: Lens.Lens' DescribeServices (Prelude.Maybe [Prelude.Text])
describeServices_serviceCodeList = Lens.lens (\DescribeServices' {serviceCodeList} -> serviceCodeList) (\s@DescribeServices' {} a -> s {serviceCodeList = a} :: DescribeServices) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeServices where
  type
    AWSResponse DescribeServices =
      DescribeServicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Prelude.<$> (x Data..?> "services" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServices where
  hashWithSalt _salt DescribeServices' {..} =
    _salt
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` serviceCodeList

instance Prelude.NFData DescribeServices where
  rnf DescribeServices' {..} =
    Prelude.rnf language
      `Prelude.seq` Prelude.rnf serviceCodeList

instance Data.ToHeaders DescribeServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSupport_20130415.DescribeServices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("language" Data..=) Prelude.<$> language,
            ("serviceCodeList" Data..=)
              Prelude.<$> serviceCodeList
          ]
      )

instance Data.ToPath DescribeServices where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServices where
  toQuery = Prelude.const Prelude.mempty

-- | The list of Amazon Web Services services returned by the
-- DescribeServices operation.
--
-- /See:/ 'newDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | A JSON-formatted list of Amazon Web Services services.
    services :: Prelude.Maybe [SupportService],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'describeServicesResponse_services' - A JSON-formatted list of Amazon Web Services services.
--
-- 'httpStatus', 'describeServicesResponse_httpStatus' - The response's http status code.
newDescribeServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServicesResponse
newDescribeServicesResponse pHttpStatus_ =
  DescribeServicesResponse'
    { services =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON-formatted list of Amazon Web Services services.
describeServicesResponse_services :: Lens.Lens' DescribeServicesResponse (Prelude.Maybe [SupportService])
describeServicesResponse_services = Lens.lens (\DescribeServicesResponse' {services} -> services) (\s@DescribeServicesResponse' {} a -> s {services = a} :: DescribeServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServicesResponse_httpStatus :: Lens.Lens' DescribeServicesResponse Prelude.Int
describeServicesResponse_httpStatus = Lens.lens (\DescribeServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeServicesResponse' {} a -> s {httpStatus = a} :: DescribeServicesResponse)

instance Prelude.NFData DescribeServicesResponse where
  rnf DescribeServicesResponse' {..} =
    Prelude.rnf services
      `Prelude.seq` Prelude.rnf httpStatus
