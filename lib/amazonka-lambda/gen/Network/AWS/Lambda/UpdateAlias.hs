{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
module Network.AWS.Lambda.UpdateAlias
    (
    -- * Creating a request
      UpdateAlias (..)
    , mkUpdateAlias
    -- ** Request lenses
    , uaFunctionName
    , uaName
    , uaDescription
    , uaFunctionVersion
    , uaRevisionId
    , uaRoutingConfig

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

-- | /See:/ 'mkUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
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
  , name :: Types.Name
    -- ^ The name of the alias.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the alias.
  , functionVersion :: Core.Maybe Types.Version
    -- ^ The function version that the alias invokes.
  , revisionId :: Core.Maybe Core.Text
    -- ^ Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
  , routingConfig :: Core.Maybe Types.AliasRoutingConfiguration
    -- ^ The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAlias' value with any optional fields omitted.
mkUpdateAlias
    :: Types.FunctionName -- ^ 'functionName'
    -> Types.Name -- ^ 'name'
    -> UpdateAlias
mkUpdateAlias functionName name
  = UpdateAlias'{functionName, name, description = Core.Nothing,
                 functionVersion = Core.Nothing, revisionId = Core.Nothing,
                 routingConfig = Core.Nothing}

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
uaFunctionName :: Lens.Lens' UpdateAlias Types.FunctionName
uaFunctionName = Lens.field @"functionName"
{-# INLINEABLE uaFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateAlias Types.Name
uaName = Lens.field @"name"
{-# INLINEABLE uaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateAlias (Core.Maybe Types.Description)
uaDescription = Lens.field @"description"
{-# INLINEABLE uaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The function version that the alias invokes.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaFunctionVersion :: Lens.Lens' UpdateAlias (Core.Maybe Types.Version)
uaFunctionVersion = Lens.field @"functionVersion"
{-# INLINEABLE uaFunctionVersion #-}
{-# DEPRECATED functionVersion "Use generic-lens or generic-optics with 'functionVersion' instead"  #-}

-- | Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRevisionId :: Lens.Lens' UpdateAlias (Core.Maybe Core.Text)
uaRevisionId = Lens.field @"revisionId"
{-# INLINEABLE uaRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
--
-- /Note:/ Consider using 'routingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoutingConfig :: Lens.Lens' UpdateAlias (Core.Maybe Types.AliasRoutingConfiguration)
uaRoutingConfig = Lens.field @"routingConfig"
{-# INLINEABLE uaRoutingConfig #-}
{-# DEPRECATED routingConfig "Use generic-lens or generic-optics with 'routingConfig' instead"  #-}

instance Core.ToQuery UpdateAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateAlias where
        toJSON UpdateAlias{..}
          = Core.object
              (Core.catMaybes
                 [("Description" Core..=) Core.<$> description,
                  ("FunctionVersion" Core..=) Core.<$> functionVersion,
                  ("RevisionId" Core..=) Core.<$> revisionId,
                  ("RoutingConfig" Core..=) Core.<$> routingConfig])

instance Core.AWSRequest UpdateAlias where
        type Rs UpdateAlias = Types.AliasConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/aliases/"
                             Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
