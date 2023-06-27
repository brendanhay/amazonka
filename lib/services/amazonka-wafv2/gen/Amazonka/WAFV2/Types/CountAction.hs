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
-- Module      : Amazonka.WAFV2.Types.CountAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CountAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomRequestHandling

-- | Specifies that WAF should count the request. Optionally defines
-- additional custom handling for the request.
--
-- This is used in the context of other settings, for example to specify
-- values for RuleAction and web ACL DefaultAction.
--
-- /See:/ 'newCountAction' smart constructor.
data CountAction = CountAction'
  { -- | Defines custom handling for the web request.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide/.
    customRequestHandling :: Prelude.Maybe CustomRequestHandling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRequestHandling', 'countAction_customRequestHandling' - Defines custom handling for the web request.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
newCountAction ::
  CountAction
newCountAction =
  CountAction'
    { customRequestHandling =
        Prelude.Nothing
    }

-- | Defines custom handling for the web request.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
countAction_customRequestHandling :: Lens.Lens' CountAction (Prelude.Maybe CustomRequestHandling)
countAction_customRequestHandling = Lens.lens (\CountAction' {customRequestHandling} -> customRequestHandling) (\s@CountAction' {} a -> s {customRequestHandling = a} :: CountAction)

instance Data.FromJSON CountAction where
  parseJSON =
    Data.withObject
      "CountAction"
      ( \x ->
          CountAction'
            Prelude.<$> (x Data..:? "CustomRequestHandling")
      )

instance Prelude.Hashable CountAction where
  hashWithSalt _salt CountAction' {..} =
    _salt `Prelude.hashWithSalt` customRequestHandling

instance Prelude.NFData CountAction where
  rnf CountAction' {..} =
    Prelude.rnf customRequestHandling

instance Data.ToJSON CountAction where
  toJSON CountAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomRequestHandling" Data..=)
              Prelude.<$> customRequestHandling
          ]
      )
