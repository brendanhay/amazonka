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
-- Module      : Amazonka.WAFV2.Types.AssociationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AssociationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.AssociatedResourceType
import Amazonka.WAFV2.Types.RequestBodyAssociatedResourceTypeConfig

-- | Specifies custom configurations for the associations between the web ACL
-- and protected resources.
--
-- Use this to customize the maximum size of the request body that your
-- protected CloudFront distributions forward to WAF for inspection. The
-- default is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
--
-- /See:/ 'newAssociationConfig' smart constructor.
data AssociationConfig = AssociationConfig'
  { -- | Customizes the maximum size of the request body that your protected
    -- CloudFront distributions forward to WAF for inspection. The default size
    -- is 16 KB (16,384 kilobytes).
    --
    -- You are charged additional fees when your protected resources forward
    -- body sizes that are larger than the default. For more information, see
    -- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
    requestBody :: Prelude.Maybe (Prelude.HashMap AssociatedResourceType RequestBodyAssociatedResourceTypeConfig)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestBody', 'associationConfig_requestBody' - Customizes the maximum size of the request body that your protected
-- CloudFront distributions forward to WAF for inspection. The default size
-- is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
newAssociationConfig ::
  AssociationConfig
newAssociationConfig =
  AssociationConfig' {requestBody = Prelude.Nothing}

-- | Customizes the maximum size of the request body that your protected
-- CloudFront distributions forward to WAF for inspection. The default size
-- is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
associationConfig_requestBody :: Lens.Lens' AssociationConfig (Prelude.Maybe (Prelude.HashMap AssociatedResourceType RequestBodyAssociatedResourceTypeConfig))
associationConfig_requestBody = Lens.lens (\AssociationConfig' {requestBody} -> requestBody) (\s@AssociationConfig' {} a -> s {requestBody = a} :: AssociationConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AssociationConfig where
  parseJSON =
    Data.withObject
      "AssociationConfig"
      ( \x ->
          AssociationConfig'
            Prelude.<$> (x Data..:? "RequestBody" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AssociationConfig where
  hashWithSalt _salt AssociationConfig' {..} =
    _salt `Prelude.hashWithSalt` requestBody

instance Prelude.NFData AssociationConfig where
  rnf AssociationConfig' {..} = Prelude.rnf requestBody

instance Data.ToJSON AssociationConfig where
  toJSON AssociationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("RequestBody" Data..=) Prelude.<$> requestBody]
      )
