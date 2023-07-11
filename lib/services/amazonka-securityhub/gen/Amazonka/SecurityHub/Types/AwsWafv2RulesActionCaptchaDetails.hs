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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2RulesActionCaptchaDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2RulesActionCaptchaDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails

-- | Specifies that WAF should run a CAPTCHA check against the request.
--
-- /See:/ 'newAwsWafv2RulesActionCaptchaDetails' smart constructor.
data AwsWafv2RulesActionCaptchaDetails = AwsWafv2RulesActionCaptchaDetails'
  { -- | Defines custom handling for the web request, used when the CAPTCHA
    -- inspection determines that the request\'s token is valid and unexpired.
    -- For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide./.
    customRequestHandling :: Prelude.Maybe AwsWafv2CustomRequestHandlingDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2RulesActionCaptchaDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRequestHandling', 'awsWafv2RulesActionCaptchaDetails_customRequestHandling' - Defines custom handling for the web request, used when the CAPTCHA
-- inspection determines that the request\'s token is valid and unexpired.
-- For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
newAwsWafv2RulesActionCaptchaDetails ::
  AwsWafv2RulesActionCaptchaDetails
newAwsWafv2RulesActionCaptchaDetails =
  AwsWafv2RulesActionCaptchaDetails'
    { customRequestHandling =
        Prelude.Nothing
    }

-- | Defines custom handling for the web request, used when the CAPTCHA
-- inspection determines that the request\'s token is valid and unexpired.
-- For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
awsWafv2RulesActionCaptchaDetails_customRequestHandling :: Lens.Lens' AwsWafv2RulesActionCaptchaDetails (Prelude.Maybe AwsWafv2CustomRequestHandlingDetails)
awsWafv2RulesActionCaptchaDetails_customRequestHandling = Lens.lens (\AwsWafv2RulesActionCaptchaDetails' {customRequestHandling} -> customRequestHandling) (\s@AwsWafv2RulesActionCaptchaDetails' {} a -> s {customRequestHandling = a} :: AwsWafv2RulesActionCaptchaDetails)

instance
  Data.FromJSON
    AwsWafv2RulesActionCaptchaDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafv2RulesActionCaptchaDetails"
      ( \x ->
          AwsWafv2RulesActionCaptchaDetails'
            Prelude.<$> (x Data..:? "CustomRequestHandling")
      )

instance
  Prelude.Hashable
    AwsWafv2RulesActionCaptchaDetails
  where
  hashWithSalt
    _salt
    AwsWafv2RulesActionCaptchaDetails' {..} =
      _salt `Prelude.hashWithSalt` customRequestHandling

instance
  Prelude.NFData
    AwsWafv2RulesActionCaptchaDetails
  where
  rnf AwsWafv2RulesActionCaptchaDetails' {..} =
    Prelude.rnf customRequestHandling

instance
  Data.ToJSON
    AwsWafv2RulesActionCaptchaDetails
  where
  toJSON AwsWafv2RulesActionCaptchaDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomRequestHandling" Data..=)
              Prelude.<$> customRequestHandling
          ]
      )
