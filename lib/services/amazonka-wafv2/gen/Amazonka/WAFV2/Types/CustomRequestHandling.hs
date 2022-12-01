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
-- Module      : Amazonka.WAFV2.Types.CustomRequestHandling
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CustomRequestHandling where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomHTTPHeader

-- | Custom request handling behavior that inserts custom headers into a web
-- request. You can add custom request handling for WAF to use when the
-- rule action doesn\'t block the request. For example, @CaptchaAction@ for
-- requests with valid t okens, and @AllowAction@.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- /See:/ 'newCustomRequestHandling' smart constructor.
data CustomRequestHandling = CustomRequestHandling'
  { -- | The HTTP headers to insert into the request. Duplicate header names are
    -- not allowed.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    insertHeaders :: Prelude.NonEmpty CustomHTTPHeader
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRequestHandling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insertHeaders', 'customRequestHandling_insertHeaders' - The HTTP headers to insert into the request. Duplicate header names are
-- not allowed.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
newCustomRequestHandling ::
  -- | 'insertHeaders'
  Prelude.NonEmpty CustomHTTPHeader ->
  CustomRequestHandling
newCustomRequestHandling pInsertHeaders_ =
  CustomRequestHandling'
    { insertHeaders =
        Lens.coerced Lens.# pInsertHeaders_
    }

-- | The HTTP headers to insert into the request. Duplicate header names are
-- not allowed.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
customRequestHandling_insertHeaders :: Lens.Lens' CustomRequestHandling (Prelude.NonEmpty CustomHTTPHeader)
customRequestHandling_insertHeaders = Lens.lens (\CustomRequestHandling' {insertHeaders} -> insertHeaders) (\s@CustomRequestHandling' {} a -> s {insertHeaders = a} :: CustomRequestHandling) Prelude.. Lens.coerced

instance Core.FromJSON CustomRequestHandling where
  parseJSON =
    Core.withObject
      "CustomRequestHandling"
      ( \x ->
          CustomRequestHandling'
            Prelude.<$> (x Core..: "InsertHeaders")
      )

instance Prelude.Hashable CustomRequestHandling where
  hashWithSalt _salt CustomRequestHandling' {..} =
    _salt `Prelude.hashWithSalt` insertHeaders

instance Prelude.NFData CustomRequestHandling where
  rnf CustomRequestHandling' {..} =
    Prelude.rnf insertHeaders

instance Core.ToJSON CustomRequestHandling where
  toJSON CustomRequestHandling' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InsertHeaders" Core..= insertHeaders)
          ]
      )
