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
-- Module      : Amazonka.WAFV2.Types.BlockAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.BlockAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomResponse

-- | Specifies that WAF should block the request and optionally defines
-- additional custom handling for the response to the web request.
--
-- This is used in the context of other settings, for example to specify
-- values for RuleAction and web ACL DefaultAction.
--
-- /See:/ 'newBlockAction' smart constructor.
data BlockAction = BlockAction'
  { -- | Defines a custom response for the web request.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    customResponse :: Prelude.Maybe CustomResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlockAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customResponse', 'blockAction_customResponse' - Defines a custom response for the web request.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
newBlockAction ::
  BlockAction
newBlockAction =
  BlockAction' {customResponse = Prelude.Nothing}

-- | Defines a custom response for the web request.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
blockAction_customResponse :: Lens.Lens' BlockAction (Prelude.Maybe CustomResponse)
blockAction_customResponse = Lens.lens (\BlockAction' {customResponse} -> customResponse) (\s@BlockAction' {} a -> s {customResponse = a} :: BlockAction)

instance Data.FromJSON BlockAction where
  parseJSON =
    Data.withObject
      "BlockAction"
      ( \x ->
          BlockAction'
            Prelude.<$> (x Data..:? "CustomResponse")
      )

instance Prelude.Hashable BlockAction where
  hashWithSalt _salt BlockAction' {..} =
    _salt `Prelude.hashWithSalt` customResponse

instance Prelude.NFData BlockAction where
  rnf BlockAction' {..} = Prelude.rnf customResponse

instance Data.ToJSON BlockAction where
  toJSON BlockAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomResponse" Data..=)
              Prelude.<$> customResponse
          ]
      )
