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
-- Module      : Amazonka.WAFV2.Types.RequestBodyAssociatedResourceTypeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RequestBodyAssociatedResourceTypeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.SizeInspectionLimit

-- | Customizes the maximum size of the request body that your protected
-- CloudFront distributions forward to WAF for inspection. The default size
-- is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
--
-- This is used in the @AssociationConfig@ of the web ACL.
--
-- /See:/ 'newRequestBodyAssociatedResourceTypeConfig' smart constructor.
data RequestBodyAssociatedResourceTypeConfig = RequestBodyAssociatedResourceTypeConfig'
  { -- | Specifies the maximum size of the web request body component that an
    -- associated CloudFront distribution should send to WAF for inspection.
    -- This applies to statements in the web ACL that inspect the body or JSON
    -- body.
    --
    -- Default: @16 KB (16,384 kilobytes)@
    defaultSizeInspectionLimit :: SizeInspectionLimit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestBodyAssociatedResourceTypeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultSizeInspectionLimit', 'requestBodyAssociatedResourceTypeConfig_defaultSizeInspectionLimit' - Specifies the maximum size of the web request body component that an
-- associated CloudFront distribution should send to WAF for inspection.
-- This applies to statements in the web ACL that inspect the body or JSON
-- body.
--
-- Default: @16 KB (16,384 kilobytes)@
newRequestBodyAssociatedResourceTypeConfig ::
  -- | 'defaultSizeInspectionLimit'
  SizeInspectionLimit ->
  RequestBodyAssociatedResourceTypeConfig
newRequestBodyAssociatedResourceTypeConfig
  pDefaultSizeInspectionLimit_ =
    RequestBodyAssociatedResourceTypeConfig'
      { defaultSizeInspectionLimit =
          pDefaultSizeInspectionLimit_
      }

-- | Specifies the maximum size of the web request body component that an
-- associated CloudFront distribution should send to WAF for inspection.
-- This applies to statements in the web ACL that inspect the body or JSON
-- body.
--
-- Default: @16 KB (16,384 kilobytes)@
requestBodyAssociatedResourceTypeConfig_defaultSizeInspectionLimit :: Lens.Lens' RequestBodyAssociatedResourceTypeConfig SizeInspectionLimit
requestBodyAssociatedResourceTypeConfig_defaultSizeInspectionLimit = Lens.lens (\RequestBodyAssociatedResourceTypeConfig' {defaultSizeInspectionLimit} -> defaultSizeInspectionLimit) (\s@RequestBodyAssociatedResourceTypeConfig' {} a -> s {defaultSizeInspectionLimit = a} :: RequestBodyAssociatedResourceTypeConfig)

instance
  Data.FromJSON
    RequestBodyAssociatedResourceTypeConfig
  where
  parseJSON =
    Data.withObject
      "RequestBodyAssociatedResourceTypeConfig"
      ( \x ->
          RequestBodyAssociatedResourceTypeConfig'
            Prelude.<$> (x Data..: "DefaultSizeInspectionLimit")
      )

instance
  Prelude.Hashable
    RequestBodyAssociatedResourceTypeConfig
  where
  hashWithSalt
    _salt
    RequestBodyAssociatedResourceTypeConfig' {..} =
      _salt
        `Prelude.hashWithSalt` defaultSizeInspectionLimit

instance
  Prelude.NFData
    RequestBodyAssociatedResourceTypeConfig
  where
  rnf RequestBodyAssociatedResourceTypeConfig' {..} =
    Prelude.rnf defaultSizeInspectionLimit

instance
  Data.ToJSON
    RequestBodyAssociatedResourceTypeConfig
  where
  toJSON RequestBodyAssociatedResourceTypeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DefaultSizeInspectionLimit"
                  Data..= defaultSizeInspectionLimit
              )
          ]
      )
