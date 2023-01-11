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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2ActionAllowDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2ActionAllowDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails

-- | Specifies that WAF should allow the request and optionally defines
-- additional custom handling for the request.
--
-- /See:/ 'newAwsWafv2ActionAllowDetails' smart constructor.
data AwsWafv2ActionAllowDetails = AwsWafv2ActionAllowDetails'
  { -- | Defines custom handling for the web request. For information about
    -- customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide./.
    customRequestHandling :: Prelude.Maybe AwsWafv2CustomRequestHandlingDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2ActionAllowDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRequestHandling', 'awsWafv2ActionAllowDetails_customRequestHandling' - Defines custom handling for the web request. For information about
-- customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
newAwsWafv2ActionAllowDetails ::
  AwsWafv2ActionAllowDetails
newAwsWafv2ActionAllowDetails =
  AwsWafv2ActionAllowDetails'
    { customRequestHandling =
        Prelude.Nothing
    }

-- | Defines custom handling for the web request. For information about
-- customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
awsWafv2ActionAllowDetails_customRequestHandling :: Lens.Lens' AwsWafv2ActionAllowDetails (Prelude.Maybe AwsWafv2CustomRequestHandlingDetails)
awsWafv2ActionAllowDetails_customRequestHandling = Lens.lens (\AwsWafv2ActionAllowDetails' {customRequestHandling} -> customRequestHandling) (\s@AwsWafv2ActionAllowDetails' {} a -> s {customRequestHandling = a} :: AwsWafv2ActionAllowDetails)

instance Data.FromJSON AwsWafv2ActionAllowDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2ActionAllowDetails"
      ( \x ->
          AwsWafv2ActionAllowDetails'
            Prelude.<$> (x Data..:? "CustomRequestHandling")
      )

instance Prelude.Hashable AwsWafv2ActionAllowDetails where
  hashWithSalt _salt AwsWafv2ActionAllowDetails' {..} =
    _salt `Prelude.hashWithSalt` customRequestHandling

instance Prelude.NFData AwsWafv2ActionAllowDetails where
  rnf AwsWafv2ActionAllowDetails' {..} =
    Prelude.rnf customRequestHandling

instance Data.ToJSON AwsWafv2ActionAllowDetails where
  toJSON AwsWafv2ActionAllowDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomRequestHandling" Data..=)
              Prelude.<$> customRequestHandling
          ]
      )
