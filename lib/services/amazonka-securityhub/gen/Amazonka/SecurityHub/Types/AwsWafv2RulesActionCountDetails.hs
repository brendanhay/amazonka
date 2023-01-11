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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2RulesActionCountDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2RulesActionCountDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails

-- | Specifies that WAF should count the request.
--
-- /See:/ 'newAwsWafv2RulesActionCountDetails' smart constructor.
data AwsWafv2RulesActionCountDetails = AwsWafv2RulesActionCountDetails'
  { -- | Defines custom handling for the web request. For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide./.
    customRequestHandling :: Prelude.Maybe AwsWafv2CustomRequestHandlingDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2RulesActionCountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRequestHandling', 'awsWafv2RulesActionCountDetails_customRequestHandling' - Defines custom handling for the web request. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
newAwsWafv2RulesActionCountDetails ::
  AwsWafv2RulesActionCountDetails
newAwsWafv2RulesActionCountDetails =
  AwsWafv2RulesActionCountDetails'
    { customRequestHandling =
        Prelude.Nothing
    }

-- | Defines custom handling for the web request. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
awsWafv2RulesActionCountDetails_customRequestHandling :: Lens.Lens' AwsWafv2RulesActionCountDetails (Prelude.Maybe AwsWafv2CustomRequestHandlingDetails)
awsWafv2RulesActionCountDetails_customRequestHandling = Lens.lens (\AwsWafv2RulesActionCountDetails' {customRequestHandling} -> customRequestHandling) (\s@AwsWafv2RulesActionCountDetails' {} a -> s {customRequestHandling = a} :: AwsWafv2RulesActionCountDetails)

instance
  Data.FromJSON
    AwsWafv2RulesActionCountDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafv2RulesActionCountDetails"
      ( \x ->
          AwsWafv2RulesActionCountDetails'
            Prelude.<$> (x Data..:? "CustomRequestHandling")
      )

instance
  Prelude.Hashable
    AwsWafv2RulesActionCountDetails
  where
  hashWithSalt
    _salt
    AwsWafv2RulesActionCountDetails' {..} =
      _salt `Prelude.hashWithSalt` customRequestHandling

instance
  Prelude.NFData
    AwsWafv2RulesActionCountDetails
  where
  rnf AwsWafv2RulesActionCountDetails' {..} =
    Prelude.rnf customRequestHandling

instance Data.ToJSON AwsWafv2RulesActionCountDetails where
  toJSON AwsWafv2RulesActionCountDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomRequestHandling" Data..=)
              Prelude.<$> customRequestHandling
          ]
      )
