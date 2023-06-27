{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2CustomResponseDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2CustomResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2CustomHttpHeader

-- | A custom response to send to the client. You can define a custom
-- response for rule actions and default web ACL actions that are set to
-- block.
--
-- /See:/ 'newAwsWafv2CustomResponseDetails' smart constructor.
data AwsWafv2CustomResponseDetails = AwsWafv2CustomResponseDetails'
  { -- | References the response body that you want WAF to return to the web
    -- request client. You can define a custom response for a rule action or a
    -- default web ACL action that is set to block.
    customResponseBodyKey :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status code to return to the client. For a list of status codes
    -- that you can use in your custom responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/customizing-the-response-status-codes.html Supported status codes for custom response>
    -- in the /WAF Developer Guide./
    responseCode :: Prelude.Maybe Prelude.Int,
    -- | The HTTP headers to use in the response.
    responseHeaders :: Prelude.Maybe [AwsWafv2CustomHttpHeader]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2CustomResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customResponseBodyKey', 'awsWafv2CustomResponseDetails_customResponseBodyKey' - References the response body that you want WAF to return to the web
-- request client. You can define a custom response for a rule action or a
-- default web ACL action that is set to block.
--
-- 'responseCode', 'awsWafv2CustomResponseDetails_responseCode' - The HTTP status code to return to the client. For a list of status codes
-- that you can use in your custom responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/customizing-the-response-status-codes.html Supported status codes for custom response>
-- in the /WAF Developer Guide./
--
-- 'responseHeaders', 'awsWafv2CustomResponseDetails_responseHeaders' - The HTTP headers to use in the response.
newAwsWafv2CustomResponseDetails ::
  AwsWafv2CustomResponseDetails
newAwsWafv2CustomResponseDetails =
  AwsWafv2CustomResponseDetails'
    { customResponseBodyKey =
        Prelude.Nothing,
      responseCode = Prelude.Nothing,
      responseHeaders = Prelude.Nothing
    }

-- | References the response body that you want WAF to return to the web
-- request client. You can define a custom response for a rule action or a
-- default web ACL action that is set to block.
awsWafv2CustomResponseDetails_customResponseBodyKey :: Lens.Lens' AwsWafv2CustomResponseDetails (Prelude.Maybe Prelude.Text)
awsWafv2CustomResponseDetails_customResponseBodyKey = Lens.lens (\AwsWafv2CustomResponseDetails' {customResponseBodyKey} -> customResponseBodyKey) (\s@AwsWafv2CustomResponseDetails' {} a -> s {customResponseBodyKey = a} :: AwsWafv2CustomResponseDetails)

-- | The HTTP status code to return to the client. For a list of status codes
-- that you can use in your custom responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/customizing-the-response-status-codes.html Supported status codes for custom response>
-- in the /WAF Developer Guide./
awsWafv2CustomResponseDetails_responseCode :: Lens.Lens' AwsWafv2CustomResponseDetails (Prelude.Maybe Prelude.Int)
awsWafv2CustomResponseDetails_responseCode = Lens.lens (\AwsWafv2CustomResponseDetails' {responseCode} -> responseCode) (\s@AwsWafv2CustomResponseDetails' {} a -> s {responseCode = a} :: AwsWafv2CustomResponseDetails)

-- | The HTTP headers to use in the response.
awsWafv2CustomResponseDetails_responseHeaders :: Lens.Lens' AwsWafv2CustomResponseDetails (Prelude.Maybe [AwsWafv2CustomHttpHeader])
awsWafv2CustomResponseDetails_responseHeaders = Lens.lens (\AwsWafv2CustomResponseDetails' {responseHeaders} -> responseHeaders) (\s@AwsWafv2CustomResponseDetails' {} a -> s {responseHeaders = a} :: AwsWafv2CustomResponseDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsWafv2CustomResponseDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2CustomResponseDetails"
      ( \x ->
          AwsWafv2CustomResponseDetails'
            Prelude.<$> (x Data..:? "CustomResponseBodyKey")
            Prelude.<*> (x Data..:? "ResponseCode")
            Prelude.<*> ( x
                            Data..:? "ResponseHeaders"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsWafv2CustomResponseDetails
  where
  hashWithSalt _salt AwsWafv2CustomResponseDetails' {..} =
    _salt
      `Prelude.hashWithSalt` customResponseBodyKey
      `Prelude.hashWithSalt` responseCode
      `Prelude.hashWithSalt` responseHeaders

instance Prelude.NFData AwsWafv2CustomResponseDetails where
  rnf AwsWafv2CustomResponseDetails' {..} =
    Prelude.rnf customResponseBodyKey
      `Prelude.seq` Prelude.rnf responseCode
      `Prelude.seq` Prelude.rnf responseHeaders

instance Data.ToJSON AwsWafv2CustomResponseDetails where
  toJSON AwsWafv2CustomResponseDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomResponseBodyKey" Data..=)
              Prelude.<$> customResponseBodyKey,
            ("ResponseCode" Data..=) Prelude.<$> responseCode,
            ("ResponseHeaders" Data..=)
              Prelude.<$> responseHeaders
          ]
      )
