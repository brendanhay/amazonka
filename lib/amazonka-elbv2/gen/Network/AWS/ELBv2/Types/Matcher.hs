{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Matcher
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Matcher
  ( Matcher (..),

    -- * Smart constructor
    mkMatcher,

    -- * Lenses
    mHTTPCode,
    mGrpcCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The codes to use when checking for a successful response from a target. If the protocol version is gRPC, these are gRPC codes. Otherwise, these are HTTP codes.
--
-- /See:/ 'mkMatcher' smart constructor.
data Matcher = Matcher'
  { hTTPCode :: Lude.Maybe Lude.Text,
    grpcCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Matcher' with the minimum fields required to make a request.
--
-- * 'grpcCode' - You can specify values between 0 and 99. You can specify multiple values (for example, "0,1") or a range of values (for example, "0-5"). The default value is 12.
-- * 'hTTPCode' - For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be "200–399".
mkMatcher ::
  Matcher
mkMatcher =
  Matcher' {hTTPCode = Lude.Nothing, grpcCode = Lude.Nothing}

-- | For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be "200–399".
--
-- /Note:/ Consider using 'hTTPCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mHTTPCode :: Lens.Lens' Matcher (Lude.Maybe Lude.Text)
mHTTPCode = Lens.lens (hTTPCode :: Matcher -> Lude.Maybe Lude.Text) (\s a -> s {hTTPCode = a} :: Matcher)
{-# DEPRECATED mHTTPCode "Use generic-lens or generic-optics with 'hTTPCode' instead." #-}

-- | You can specify values between 0 and 99. You can specify multiple values (for example, "0,1") or a range of values (for example, "0-5"). The default value is 12.
--
-- /Note:/ Consider using 'grpcCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mGrpcCode :: Lens.Lens' Matcher (Lude.Maybe Lude.Text)
mGrpcCode = Lens.lens (grpcCode :: Matcher -> Lude.Maybe Lude.Text) (\s a -> s {grpcCode = a} :: Matcher)
{-# DEPRECATED mGrpcCode "Use generic-lens or generic-optics with 'grpcCode' instead." #-}

instance Lude.FromXML Matcher where
  parseXML x =
    Matcher'
      Lude.<$> (x Lude..@? "HttpCode") Lude.<*> (x Lude..@? "GrpcCode")

instance Lude.ToQuery Matcher where
  toQuery Matcher' {..} =
    Lude.mconcat
      ["HttpCode" Lude.=: hTTPCode, "GrpcCode" Lude.=: grpcCode]
