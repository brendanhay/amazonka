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
-- Module      : Network.AWS.ELBv2.Types.Matcher
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Matcher where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The codes to use when checking for a successful response from a target.
-- If the protocol version is gRPC, these are gRPC codes. Otherwise, these
-- are HTTP codes.
--
-- /See:/ 'newMatcher' smart constructor.
data Matcher = Matcher'
  { -- | You can specify values between 0 and 99. You can specify multiple values
    -- (for example, \"0,1\") or a range of values (for example, \"0-5\"). The
    -- default value is 12.
    grpcCode :: Core.Maybe Core.Text,
    -- | For Application Load Balancers, you can specify values between 200 and
    -- 499, and the default value is 200. You can specify multiple values (for
    -- example, \"200,202\") or a range of values (for example, \"200-299\").
    --
    -- For Network Load Balancers and Gateway Load Balancers, this must be
    -- \"200–399\".
    httpCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Matcher' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grpcCode', 'matcher_grpcCode' - You can specify values between 0 and 99. You can specify multiple values
-- (for example, \"0,1\") or a range of values (for example, \"0-5\"). The
-- default value is 12.
--
-- 'httpCode', 'matcher_httpCode' - For Application Load Balancers, you can specify values between 200 and
-- 499, and the default value is 200. You can specify multiple values (for
-- example, \"200,202\") or a range of values (for example, \"200-299\").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be
-- \"200–399\".
newMatcher ::
  Matcher
newMatcher =
  Matcher'
    { grpcCode = Core.Nothing,
      httpCode = Core.Nothing
    }

-- | You can specify values between 0 and 99. You can specify multiple values
-- (for example, \"0,1\") or a range of values (for example, \"0-5\"). The
-- default value is 12.
matcher_grpcCode :: Lens.Lens' Matcher (Core.Maybe Core.Text)
matcher_grpcCode = Lens.lens (\Matcher' {grpcCode} -> grpcCode) (\s@Matcher' {} a -> s {grpcCode = a} :: Matcher)

-- | For Application Load Balancers, you can specify values between 200 and
-- 499, and the default value is 200. You can specify multiple values (for
-- example, \"200,202\") or a range of values (for example, \"200-299\").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be
-- \"200–399\".
matcher_httpCode :: Lens.Lens' Matcher (Core.Maybe Core.Text)
matcher_httpCode = Lens.lens (\Matcher' {httpCode} -> httpCode) (\s@Matcher' {} a -> s {httpCode = a} :: Matcher)

instance Core.FromXML Matcher where
  parseXML x =
    Matcher'
      Core.<$> (x Core..@? "GrpcCode")
      Core.<*> (x Core..@? "HttpCode")

instance Core.Hashable Matcher

instance Core.NFData Matcher

instance Core.ToQuery Matcher where
  toQuery Matcher' {..} =
    Core.mconcat
      [ "GrpcCode" Core.=: grpcCode,
        "HttpCode" Core.=: httpCode
      ]
