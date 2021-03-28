{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a statement to a topic's access control policy, granting access for the specified AWS accounts to the specified actions.
module Network.AWS.SNS.AddPermission
    (
    -- * Creating a request
      AddPermission (..)
    , mkAddPermission
    -- ** Request lenses
    , apTopicArn
    , apLabel
    , apAWSAccountId
    , apActionName

    -- * Destructuring the response
    , AddPermissionResponse (..)
    , mkAddPermissionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | /See:/ 'mkAddPermission' smart constructor.
data AddPermission = AddPermission'
  { topicArn :: Types.TopicArn
    -- ^ The ARN of the topic whose access control policy you wish to modify.
  , label :: Types.Label
    -- ^ A unique identifier for the new policy statement.
  , aWSAccountId :: [Types.Delegate]
    -- ^ The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
  , actionName :: [Types.Action]
    -- ^ The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPermission' value with any optional fields omitted.
mkAddPermission
    :: Types.TopicArn -- ^ 'topicArn'
    -> Types.Label -- ^ 'label'
    -> AddPermission
mkAddPermission topicArn label
  = AddPermission'{topicArn, label, aWSAccountId = Core.mempty,
                   actionName = Core.mempty}

-- | The ARN of the topic whose access control policy you wish to modify.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTopicArn :: Lens.Lens' AddPermission Types.TopicArn
apTopicArn = Lens.field @"topicArn"
{-# INLINEABLE apTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | A unique identifier for the new policy statement.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apLabel :: Lens.Lens' AddPermission Types.Label
apLabel = Lens.field @"label"
{-# INLINEABLE apLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
--
-- /Note:/ Consider using 'aWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAWSAccountId :: Lens.Lens' AddPermission [Types.Delegate]
apAWSAccountId = Lens.field @"aWSAccountId"
{-# INLINEABLE apAWSAccountId #-}
{-# DEPRECATED aWSAccountId "Use generic-lens or generic-optics with 'aWSAccountId' instead"  #-}

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@ .
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apActionName :: Lens.Lens' AddPermission [Types.Action]
apActionName = Lens.field @"actionName"
{-# INLINEABLE apActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

instance Core.ToQuery AddPermission where
        toQuery AddPermission{..}
          = Core.toQueryPair "Action" ("AddPermission" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "TopicArn" topicArn
              Core.<> Core.toQueryPair "Label" label
              Core.<>
              Core.toQueryPair "AWSAccountId"
                (Core.toQueryList "member" aWSAccountId)
              Core.<>
              Core.toQueryPair "ActionName"
                (Core.toQueryList "member" actionName)

instance Core.ToHeaders AddPermission where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddPermission where
        type Rs AddPermission = AddPermissionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AddPermissionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPermissionResponse' value with any optional fields omitted.
mkAddPermissionResponse
    :: AddPermissionResponse
mkAddPermissionResponse = AddPermissionResponse'
