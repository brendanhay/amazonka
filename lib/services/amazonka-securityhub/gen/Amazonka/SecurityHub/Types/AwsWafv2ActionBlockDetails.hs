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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2ActionBlockDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2ActionBlockDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2CustomResponseDetails

-- | Specifies that WAF should block the request and optionally defines
-- additional custom handling for the response to the web request.
--
-- /See:/ 'newAwsWafv2ActionBlockDetails' smart constructor.
data AwsWafv2ActionBlockDetails = AwsWafv2ActionBlockDetails'
  { -- | Defines a custom response for the web request. For information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide./.
    customResponse :: Prelude.Maybe AwsWafv2CustomResponseDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2ActionBlockDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customResponse', 'awsWafv2ActionBlockDetails_customResponse' - Defines a custom response for the web request. For information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
newAwsWafv2ActionBlockDetails ::
  AwsWafv2ActionBlockDetails
newAwsWafv2ActionBlockDetails =
  AwsWafv2ActionBlockDetails'
    { customResponse =
        Prelude.Nothing
    }

-- | Defines a custom response for the web request. For information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide./.
awsWafv2ActionBlockDetails_customResponse :: Lens.Lens' AwsWafv2ActionBlockDetails (Prelude.Maybe AwsWafv2CustomResponseDetails)
awsWafv2ActionBlockDetails_customResponse = Lens.lens (\AwsWafv2ActionBlockDetails' {customResponse} -> customResponse) (\s@AwsWafv2ActionBlockDetails' {} a -> s {customResponse = a} :: AwsWafv2ActionBlockDetails)

instance Data.FromJSON AwsWafv2ActionBlockDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2ActionBlockDetails"
      ( \x ->
          AwsWafv2ActionBlockDetails'
            Prelude.<$> (x Data..:? "CustomResponse")
      )

instance Prelude.Hashable AwsWafv2ActionBlockDetails where
  hashWithSalt _salt AwsWafv2ActionBlockDetails' {..} =
    _salt `Prelude.hashWithSalt` customResponse

instance Prelude.NFData AwsWafv2ActionBlockDetails where
  rnf AwsWafv2ActionBlockDetails' {..} =
    Prelude.rnf customResponse

instance Data.ToJSON AwsWafv2ActionBlockDetails where
  toJSON AwsWafv2ActionBlockDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomResponse" Data..=)
              Prelude.<$> customResponse
          ]
      )
