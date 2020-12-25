{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SignalResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to the specified resource with a success or failure status. You can use the SignalResource API in conjunction with a creation policy or update policy. AWS CloudFormation doesn't proceed with a stack creation or update until resources receive the required number of signals or the timeout period is exceeded. The SignalResource API is useful in cases where you want to send signals from anywhere other than an Amazon EC2 instance.
module Network.AWS.CloudFormation.SignalResource
  ( -- * Creating a request
    SignalResource (..),
    mkSignalResource,

    -- ** Request lenses
    srfStackName,
    srfLogicalResourceId,
    srfUniqueId,
    srfStatus,

    -- * Destructuring the response
    SignalResourceResponse (..),
    mkSignalResourceResponse,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'SignalResource' action.
--
-- /See:/ 'mkSignalResource' smart constructor.
data SignalResource = SignalResource'
  { -- | The stack name or unique stack ID that includes the resource that you want to signal.
    stackName :: Types.StackName,
    -- | The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
    logicalResourceId :: Types.LogicalResourceId,
    -- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
    uniqueId :: Types.UniqueId,
    -- | The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
    status :: Types.ResourceSignalStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignalResource' value with any optional fields omitted.
mkSignalResource ::
  -- | 'stackName'
  Types.StackName ->
  -- | 'logicalResourceId'
  Types.LogicalResourceId ->
  -- | 'uniqueId'
  Types.UniqueId ->
  -- | 'status'
  Types.ResourceSignalStatus ->
  SignalResource
mkSignalResource stackName logicalResourceId uniqueId status =
  SignalResource' {stackName, logicalResourceId, uniqueId, status}

-- | The stack name or unique stack ID that includes the resource that you want to signal.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srfStackName :: Lens.Lens' SignalResource Types.StackName
srfStackName = Lens.field @"stackName"
{-# DEPRECATED srfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srfLogicalResourceId :: Lens.Lens' SignalResource Types.LogicalResourceId
srfLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED srfLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
--
-- /Note:/ Consider using 'uniqueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srfUniqueId :: Lens.Lens' SignalResource Types.UniqueId
srfUniqueId = Lens.field @"uniqueId"
{-# DEPRECATED srfUniqueId "Use generic-lens or generic-optics with 'uniqueId' instead." #-}

-- | The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srfStatus :: Lens.Lens' SignalResource Types.ResourceSignalStatus
srfStatus = Lens.field @"status"
{-# DEPRECATED srfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.AWSRequest SignalResource where
  type Rs SignalResource = SignalResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SignalResource")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> (Core.toQueryValue "LogicalResourceId" logicalResourceId)
                Core.<> (Core.toQueryValue "UniqueId" uniqueId)
                Core.<> (Core.toQueryValue "Status" status)
            )
      }
  response = Response.receiveNull SignalResourceResponse'

-- | /See:/ 'mkSignalResourceResponse' smart constructor.
data SignalResourceResponse = SignalResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignalResourceResponse' value with any optional fields omitted.
mkSignalResourceResponse ::
  SignalResourceResponse
mkSignalResourceResponse = SignalResourceResponse'
