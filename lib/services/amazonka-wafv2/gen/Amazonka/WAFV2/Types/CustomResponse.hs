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
-- Module      : Amazonka.WAFV2.Types.CustomResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CustomResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomHTTPHeader

-- | A custom response to send to the client. You can define a custom
-- response for rule actions and default web ACL actions that are set to
-- BlockAction.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- /See:/ 'newCustomResponse' smart constructor.
data CustomResponse = CustomResponse'
  { -- | References the response body that you want WAF to return to the web
    -- request client. You can define a custom response for a rule action or a
    -- default web ACL action that is set to block. To do this, you first
    -- define the response body key and value in the @CustomResponseBodies@
    -- setting for the WebACL or RuleGroup where you want to use it. Then, in
    -- the rule action or web ACL default action @BlockAction@ setting, you
    -- reference the response body using this key.
    customResponseBodyKey :: Prelude.Maybe Prelude.Text,
    -- | The HTTP headers to use in the response. Duplicate header names are not
    -- allowed.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    responseHeaders :: Prelude.Maybe (Prelude.NonEmpty CustomHTTPHeader),
    -- | The HTTP status code to return to the client.
    --
    -- For a list of status codes that you can use in your custom responses,
    -- see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/customizing-the-response-status-codes.html Supported status codes for custom response>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    responseCode :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customResponseBodyKey', 'customResponse_customResponseBodyKey' - References the response body that you want WAF to return to the web
-- request client. You can define a custom response for a rule action or a
-- default web ACL action that is set to block. To do this, you first
-- define the response body key and value in the @CustomResponseBodies@
-- setting for the WebACL or RuleGroup where you want to use it. Then, in
-- the rule action or web ACL default action @BlockAction@ setting, you
-- reference the response body using this key.
--
-- 'responseHeaders', 'customResponse_responseHeaders' - The HTTP headers to use in the response. Duplicate header names are not
-- allowed.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- 'responseCode', 'customResponse_responseCode' - The HTTP status code to return to the client.
--
-- For a list of status codes that you can use in your custom responses,
-- see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/customizing-the-response-status-codes.html Supported status codes for custom response>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
newCustomResponse ::
  -- | 'responseCode'
  Prelude.Natural ->
  CustomResponse
newCustomResponse pResponseCode_ =
  CustomResponse'
    { customResponseBodyKey =
        Prelude.Nothing,
      responseHeaders = Prelude.Nothing,
      responseCode = pResponseCode_
    }

-- | References the response body that you want WAF to return to the web
-- request client. You can define a custom response for a rule action or a
-- default web ACL action that is set to block. To do this, you first
-- define the response body key and value in the @CustomResponseBodies@
-- setting for the WebACL or RuleGroup where you want to use it. Then, in
-- the rule action or web ACL default action @BlockAction@ setting, you
-- reference the response body using this key.
customResponse_customResponseBodyKey :: Lens.Lens' CustomResponse (Prelude.Maybe Prelude.Text)
customResponse_customResponseBodyKey = Lens.lens (\CustomResponse' {customResponseBodyKey} -> customResponseBodyKey) (\s@CustomResponse' {} a -> s {customResponseBodyKey = a} :: CustomResponse)

-- | The HTTP headers to use in the response. Duplicate header names are not
-- allowed.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
customResponse_responseHeaders :: Lens.Lens' CustomResponse (Prelude.Maybe (Prelude.NonEmpty CustomHTTPHeader))
customResponse_responseHeaders = Lens.lens (\CustomResponse' {responseHeaders} -> responseHeaders) (\s@CustomResponse' {} a -> s {responseHeaders = a} :: CustomResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status code to return to the client.
--
-- For a list of status codes that you can use in your custom responses,
-- see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/customizing-the-response-status-codes.html Supported status codes for custom response>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
customResponse_responseCode :: Lens.Lens' CustomResponse Prelude.Natural
customResponse_responseCode = Lens.lens (\CustomResponse' {responseCode} -> responseCode) (\s@CustomResponse' {} a -> s {responseCode = a} :: CustomResponse)

instance Core.FromJSON CustomResponse where
  parseJSON =
    Core.withObject
      "CustomResponse"
      ( \x ->
          CustomResponse'
            Prelude.<$> (x Core..:? "CustomResponseBodyKey")
            Prelude.<*> (x Core..:? "ResponseHeaders")
            Prelude.<*> (x Core..: "ResponseCode")
      )

instance Prelude.Hashable CustomResponse where
  hashWithSalt _salt CustomResponse' {..} =
    _salt `Prelude.hashWithSalt` customResponseBodyKey
      `Prelude.hashWithSalt` responseHeaders
      `Prelude.hashWithSalt` responseCode

instance Prelude.NFData CustomResponse where
  rnf CustomResponse' {..} =
    Prelude.rnf customResponseBodyKey
      `Prelude.seq` Prelude.rnf responseHeaders
      `Prelude.seq` Prelude.rnf responseCode

instance Core.ToJSON CustomResponse where
  toJSON CustomResponse' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomResponseBodyKey" Core..=)
              Prelude.<$> customResponseBodyKey,
            ("ResponseHeaders" Core..=)
              Prelude.<$> responseHeaders,
            Prelude.Just ("ResponseCode" Core..= responseCode)
          ]
      )
