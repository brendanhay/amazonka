{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> for a Lambda function version. Use aliases to provide clients with a function identifier that you can update to invoke a different version.
--
-- You can also map an alias to split invocation requests between two versions. Use the @RoutingConfig@ parameter to specify a second version and the percentage of invocation requests that it receives.
module Network.AWS.Lambda.CreateAlias
    (
    -- * Creating a request
      CreateAlias (..)
    , mkCreateAlias
    -- ** Request lenses
    , caFunctionName
    , caName
    , caFunctionVersion
    , caDescription
    , caRoutingConfig

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

-- | /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
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
  , functionVersion :: Types.Version
    -- ^ The function version that the alias invokes.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the alias.
  , routingConfig :: Core.Maybe Types.AliasRoutingConfiguration
    -- ^ The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlias' value with any optional fields omitted.
mkCreateAlias
    :: Types.FunctionName -- ^ 'functionName'
    -> Types.Alias -- ^ 'name'
    -> Types.Version -- ^ 'functionVersion'
    -> CreateAlias
mkCreateAlias functionName name functionVersion
  = CreateAlias'{functionName, name, functionVersion,
                 description = Core.Nothing, routingConfig = Core.Nothing}

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
caFunctionName :: Lens.Lens' CreateAlias Types.FunctionName
caFunctionName = Lens.field @"functionName"
{-# INLINEABLE caFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateAlias Types.Alias
caName = Lens.field @"name"
{-# INLINEABLE caName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The function version that the alias invokes.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caFunctionVersion :: Lens.Lens' CreateAlias Types.Version
caFunctionVersion = Lens.field @"functionVersion"
{-# INLINEABLE caFunctionVersion #-}
{-# DEPRECATED functionVersion "Use generic-lens or generic-optics with 'functionVersion' instead"  #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateAlias (Core.Maybe Types.Description)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
--
-- /Note:/ Consider using 'routingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoutingConfig :: Lens.Lens' CreateAlias (Core.Maybe Types.AliasRoutingConfiguration)
caRoutingConfig = Lens.field @"routingConfig"
{-# INLINEABLE caRoutingConfig #-}
{-# DEPRECATED routingConfig "Use generic-lens or generic-optics with 'routingConfig' instead"  #-}

instance Core.ToQuery CreateAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateAlias where
        toJSON CreateAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("FunctionVersion" Core..= functionVersion),
                  ("Description" Core..=) Core.<$> description,
                  ("RoutingConfig" Core..=) Core.<$> routingConfig])

instance Core.AWSRequest CreateAlias where
        type Rs CreateAlias = Types.AliasConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/aliases",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
