{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Matcher
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Matcher
  ( Matcher (..)
  -- * Smart constructor
  , mkMatcher
  -- * Lenses
  , mGrpcCode
  , mHttpCode
  ) where

import qualified Network.AWS.ELBv2.Types.GrpcCode as Types
import qualified Network.AWS.ELBv2.Types.HttpCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The codes to use when checking for a successful response from a target. If the protocol version is gRPC, these are gRPC codes. Otherwise, these are HTTP codes.
--
-- /See:/ 'mkMatcher' smart constructor.
data Matcher = Matcher'
  { grpcCode :: Core.Maybe Types.GrpcCode
    -- ^ You can specify values between 0 and 99. You can specify multiple values (for example, "0,1") or a range of values (for example, "0-5"). The default value is 12.
  , httpCode :: Core.Maybe Types.HttpCode
    -- ^ For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be "200–399".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Matcher' value with any optional fields omitted.
mkMatcher
    :: Matcher
mkMatcher
  = Matcher'{grpcCode = Core.Nothing, httpCode = Core.Nothing}

-- | You can specify values between 0 and 99. You can specify multiple values (for example, "0,1") or a range of values (for example, "0-5"). The default value is 12.
--
-- /Note:/ Consider using 'grpcCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mGrpcCode :: Lens.Lens' Matcher (Core.Maybe Types.GrpcCode)
mGrpcCode = Lens.field @"grpcCode"
{-# INLINEABLE mGrpcCode #-}
{-# DEPRECATED grpcCode "Use generic-lens or generic-optics with 'grpcCode' instead"  #-}

-- | For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299").
--
-- For Network Load Balancers and Gateway Load Balancers, this must be "200–399".
--
-- /Note:/ Consider using 'httpCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mHttpCode :: Lens.Lens' Matcher (Core.Maybe Types.HttpCode)
mHttpCode = Lens.field @"httpCode"
{-# INLINEABLE mHttpCode #-}
{-# DEPRECATED httpCode "Use generic-lens or generic-optics with 'httpCode' instead"  #-}

instance Core.ToQuery Matcher where
        toQuery Matcher{..}
          = Core.maybe Core.mempty (Core.toQueryPair "GrpcCode") grpcCode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HttpCode") httpCode

instance Core.FromXML Matcher where
        parseXML x
          = Matcher' Core.<$>
              (x Core..@? "GrpcCode") Core.<*> x Core..@? "HttpCode"
