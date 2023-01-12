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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicySummary where

import Amazonka.CloudFront.Types.ResponseHeadersPolicy
import Amazonka.CloudFront.Types.ResponseHeadersPolicyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a response headers policy.
--
-- /See:/ 'newResponseHeadersPolicySummary' smart constructor.
data ResponseHeadersPolicySummary = ResponseHeadersPolicySummary'
  { -- | The type of response headers policy, either @managed@ (created by Amazon
    -- Web Services) or @custom@ (created in this Amazon Web Services account).
    type' :: ResponseHeadersPolicyType,
    -- | The response headers policy.
    responseHeadersPolicy :: ResponseHeadersPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'responseHeadersPolicySummary_type' - The type of response headers policy, either @managed@ (created by Amazon
-- Web Services) or @custom@ (created in this Amazon Web Services account).
--
-- 'responseHeadersPolicy', 'responseHeadersPolicySummary_responseHeadersPolicy' - The response headers policy.
newResponseHeadersPolicySummary ::
  -- | 'type''
  ResponseHeadersPolicyType ->
  -- | 'responseHeadersPolicy'
  ResponseHeadersPolicy ->
  ResponseHeadersPolicySummary
newResponseHeadersPolicySummary
  pType_
  pResponseHeadersPolicy_ =
    ResponseHeadersPolicySummary'
      { type' = pType_,
        responseHeadersPolicy =
          pResponseHeadersPolicy_
      }

-- | The type of response headers policy, either @managed@ (created by Amazon
-- Web Services) or @custom@ (created in this Amazon Web Services account).
responseHeadersPolicySummary_type :: Lens.Lens' ResponseHeadersPolicySummary ResponseHeadersPolicyType
responseHeadersPolicySummary_type = Lens.lens (\ResponseHeadersPolicySummary' {type'} -> type') (\s@ResponseHeadersPolicySummary' {} a -> s {type' = a} :: ResponseHeadersPolicySummary)

-- | The response headers policy.
responseHeadersPolicySummary_responseHeadersPolicy :: Lens.Lens' ResponseHeadersPolicySummary ResponseHeadersPolicy
responseHeadersPolicySummary_responseHeadersPolicy = Lens.lens (\ResponseHeadersPolicySummary' {responseHeadersPolicy} -> responseHeadersPolicy) (\s@ResponseHeadersPolicySummary' {} a -> s {responseHeadersPolicy = a} :: ResponseHeadersPolicySummary)

instance Data.FromXML ResponseHeadersPolicySummary where
  parseXML x =
    ResponseHeadersPolicySummary'
      Prelude.<$> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "ResponseHeadersPolicy")

instance
  Prelude.Hashable
    ResponseHeadersPolicySummary
  where
  hashWithSalt _salt ResponseHeadersPolicySummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` responseHeadersPolicy

instance Prelude.NFData ResponseHeadersPolicySummary where
  rnf ResponseHeadersPolicySummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf responseHeadersPolicy
