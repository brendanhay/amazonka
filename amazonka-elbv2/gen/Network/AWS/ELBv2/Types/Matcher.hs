{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The codes to use when checking for a successful response from a target.
-- If the protocol version is gRPC, these are gRPC codes. Otherwise, these
-- are HTTP codes.
--
-- /See:/ 'newMatcher' smart constructor.
data Matcher = Matcher'
  { -- | You can specify values between 0 and 99. You can specify multiple values
    -- (for example, \"0,1\") or a range of values (for example, \"0-5\"). The
    -- default value is 12.
    grpcCode :: Prelude.Maybe Prelude.Text,
    -- | For Application Load Balancers, you can specify values between 200 and
    -- 499, and the default value is 200. You can specify multiple values (for
    -- example, \"200,202\") or a range of values (for example, \"200-299\").
    --
    -- For Network Load Balancers and Gateway Load Balancers, this must be
    -- \"200–399\".
    httpCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { grpcCode = Prelude.Nothing,
      httpCode = Prelude.Nothing
    }

-- | You can specify values between 0 and 99. You can specify multiple values
-- (for example, \"0,1\") or a range of values (for example, \"0-5\"). The
-- default value is 12.
matcher_grpcCode :: Lens.Lens' Matcher (Prelude.Maybe Prelude.Text)
matcher_grpcCode = Lens.lens (\Matcher' {grpcCode} -> grpcCode) (\s@Matcher' {} a -> s {grpcCode = a} :: Matcher)

-- | For Application Load Balancers, you can specify values between 200 and
-- 499, and the default value is 200. You can specify multiple values (for
-- example, \"200,202\") or a range of values (for example, \"200-299\").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be
-- \"200–399\".
matcher_httpCode :: Lens.Lens' Matcher (Prelude.Maybe Prelude.Text)
matcher_httpCode = Lens.lens (\Matcher' {httpCode} -> httpCode) (\s@Matcher' {} a -> s {httpCode = a} :: Matcher)

instance Prelude.FromXML Matcher where
  parseXML x =
    Matcher'
      Prelude.<$> (x Prelude..@? "GrpcCode")
      Prelude.<*> (x Prelude..@? "HttpCode")

instance Prelude.Hashable Matcher

instance Prelude.NFData Matcher

instance Prelude.ToQuery Matcher where
  toQuery Matcher' {..} =
    Prelude.mconcat
      [ "GrpcCode" Prelude.=: grpcCode,
        "HttpCode" Prelude.=: httpCode
      ]
