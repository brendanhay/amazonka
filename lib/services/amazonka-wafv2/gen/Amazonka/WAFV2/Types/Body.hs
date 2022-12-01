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
-- Module      : Amazonka.WAFV2.Types.Body
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Body where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.OversizeHandling

-- | Inspect the body of the web request. The body immediately follows the
-- request headers.
--
-- This is used to indicate the web request component to inspect, in the
-- FieldToMatch specification.
--
-- /See:/ 'newBody' smart constructor.
data Body = Body'
  { -- | What WAF should do if the body is larger than WAF can inspect. WAF does
    -- not support inspecting the entire contents of the body of a web request
    -- when the body exceeds 8 KB (8192 bytes). Only the first 8 KB of the
    -- request body are forwarded to WAF by the underlying host service.
    --
    -- The options for oversize handling are the following:
    --
    -- -   @CONTINUE@ - Inspect the body normally, according to the rule
    --     inspection criteria.
    --
    -- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
    --     applies the rule action to the request.
    --
    -- -   @NO_MATCH@ - Treat the web request as not matching the rule
    --     statement.
    --
    -- You can combine the @MATCH@ or @NO_MATCH@ settings for oversize handling
    -- with your rule and web ACL action settings, so that you block any
    -- request whose body is over 8 KB.
    --
    -- Default: @CONTINUE@
    oversizeHandling :: Prelude.Maybe OversizeHandling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Body' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oversizeHandling', 'body_oversizeHandling' - What WAF should do if the body is larger than WAF can inspect. WAF does
-- not support inspecting the entire contents of the body of a web request
-- when the body exceeds 8 KB (8192 bytes). Only the first 8 KB of the
-- request body are forwarded to WAF by the underlying host service.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the body normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- You can combine the @MATCH@ or @NO_MATCH@ settings for oversize handling
-- with your rule and web ACL action settings, so that you block any
-- request whose body is over 8 KB.
--
-- Default: @CONTINUE@
newBody ::
  Body
newBody = Body' {oversizeHandling = Prelude.Nothing}

-- | What WAF should do if the body is larger than WAF can inspect. WAF does
-- not support inspecting the entire contents of the body of a web request
-- when the body exceeds 8 KB (8192 bytes). Only the first 8 KB of the
-- request body are forwarded to WAF by the underlying host service.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the body normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- You can combine the @MATCH@ or @NO_MATCH@ settings for oversize handling
-- with your rule and web ACL action settings, so that you block any
-- request whose body is over 8 KB.
--
-- Default: @CONTINUE@
body_oversizeHandling :: Lens.Lens' Body (Prelude.Maybe OversizeHandling)
body_oversizeHandling = Lens.lens (\Body' {oversizeHandling} -> oversizeHandling) (\s@Body' {} a -> s {oversizeHandling = a} :: Body)

instance Core.FromJSON Body where
  parseJSON =
    Core.withObject
      "Body"
      ( \x ->
          Body' Prelude.<$> (x Core..:? "OversizeHandling")
      )

instance Prelude.Hashable Body where
  hashWithSalt _salt Body' {..} =
    _salt `Prelude.hashWithSalt` oversizeHandling

instance Prelude.NFData Body where
  rnf Body' {..} = Prelude.rnf oversizeHandling

instance Core.ToJSON Body where
  toJSON Body' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OversizeHandling" Core..=)
              Prelude.<$> oversizeHandling
          ]
      )
