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
-- Module      : Amazonka.WAFV2.Types.CaptchaAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CaptchaAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomRequestHandling

-- | Specifies that WAF should run a @CAPTCHA@ check against the request:
--
-- -   If the request includes a valid, unexpired @CAPTCHA@ token, WAF
--     applies any custom request handling and labels that you\'ve
--     configured and then allows the web request inspection to proceed to
--     the next rule, similar to a @CountAction@.
--
-- -   If the request doesn\'t include a valid, unexpired token, WAF
--     discontinues the web ACL evaluation of the request and blocks it
--     from going to its intended destination.
--
--     WAF generates a response that it sends back to the client, which
--     includes the following:
--
--     -   The header @x-amzn-waf-action@ with a value of @captcha@.
--
--     -   The HTTP status code @405 Method Not Allowed@.
--
--     -   If the request contains an @Accept@ header with a value of
--         @text\/html@, the response includes a @CAPTCHA@ JavaScript page
--         interstitial.
--
-- You can configure the expiration time in the @CaptchaConfig@
-- @ImmunityTimeProperty@ setting at the rule and web ACL level. The rule
-- setting overrides the web ACL setting.
--
-- This action option is available for rules. It isn\'t available for web
-- ACL default actions.
--
-- /See:/ 'newCaptchaAction' smart constructor.
data CaptchaAction = CaptchaAction'
  { -- | Defines custom handling for the web request, used when the @CAPTCHA@
    -- inspection determines that the request\'s token is valid and unexpired.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    customRequestHandling :: Prelude.Maybe CustomRequestHandling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptchaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRequestHandling', 'captchaAction_customRequestHandling' - Defines custom handling for the web request, used when the @CAPTCHA@
-- inspection determines that the request\'s token is valid and unexpired.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
newCaptchaAction ::
  CaptchaAction
newCaptchaAction =
  CaptchaAction'
    { customRequestHandling =
        Prelude.Nothing
    }

-- | Defines custom handling for the web request, used when the @CAPTCHA@
-- inspection determines that the request\'s token is valid and unexpired.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
captchaAction_customRequestHandling :: Lens.Lens' CaptchaAction (Prelude.Maybe CustomRequestHandling)
captchaAction_customRequestHandling = Lens.lens (\CaptchaAction' {customRequestHandling} -> customRequestHandling) (\s@CaptchaAction' {} a -> s {customRequestHandling = a} :: CaptchaAction)

instance Data.FromJSON CaptchaAction where
  parseJSON =
    Data.withObject
      "CaptchaAction"
      ( \x ->
          CaptchaAction'
            Prelude.<$> (x Data..:? "CustomRequestHandling")
      )

instance Prelude.Hashable CaptchaAction where
  hashWithSalt _salt CaptchaAction' {..} =
    _salt `Prelude.hashWithSalt` customRequestHandling

instance Prelude.NFData CaptchaAction where
  rnf CaptchaAction' {..} =
    Prelude.rnf customRequestHandling

instance Data.ToJSON CaptchaAction where
  toJSON CaptchaAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomRequestHandling" Data..=)
              Prelude.<$> customRequestHandling
          ]
      )
