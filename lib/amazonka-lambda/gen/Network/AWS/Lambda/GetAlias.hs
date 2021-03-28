{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
module Network.AWS.Lambda.GetAlias
    (
    -- * Creating a request
      GetAlias (..)
    , mkGetAlias
    -- ** Request lenses
    , gaFunctionName
    , gaName

     -- * Destructuring the response
    , Types.AliasConfiguration (..)
    , Types.mkAliasConfiguration
    -- ** Response lenses
    , Types.acAliasArn
    , Types.acDescription
    , Types.acFunctionVersion
    , Types.acName
    , Types.acRevisionId
    , Types.acRoutingConfig
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAlias' smart constructor.
data GetAlias = GetAlias'
  { functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
  , name :: Types.Alias
    -- ^ The name of the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAlias' value with any optional fields omitted.
mkGetAlias
    :: Types.FunctionName -- ^ 'functionName'
    -> Types.Alias -- ^ 'name'
    -> GetAlias
mkGetAlias functionName name = GetAlias'{functionName, name}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaFunctionName :: Lens.Lens' GetAlias Types.FunctionName
gaFunctionName = Lens.field @"functionName"
{-# INLINEABLE gaFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaName :: Lens.Lens' GetAlias Types.Alias
gaName = Lens.field @"name"
{-# INLINEABLE gaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAlias where
        type Rs GetAlias = Types.AliasConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/aliases/"
                             Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
