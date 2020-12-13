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
    sStatus,
    sLogicalResourceId,
    sUniqueId,
    sStackName,

    -- * Destructuring the response
    SignalResourceResponse (..),
    mkSignalResourceResponse,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'SignalResource' action.
--
-- /See:/ 'mkSignalResource' smart constructor.
data SignalResource = SignalResource'
  { -- | The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
    status :: ResourceSignalStatus,
    -- | The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
    logicalResourceId :: Lude.Text,
    -- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
    uniqueId :: Lude.Text,
    -- | The stack name or unique stack ID that includes the resource that you want to signal.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SignalResource' with the minimum fields required to make a request.
--
-- * 'status' - The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
-- * 'logicalResourceId' - The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
-- * 'uniqueId' - A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
-- * 'stackName' - The stack name or unique stack ID that includes the resource that you want to signal.
mkSignalResource ::
  -- | 'status'
  ResourceSignalStatus ->
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'uniqueId'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  SignalResource
mkSignalResource
  pStatus_
  pLogicalResourceId_
  pUniqueId_
  pStackName_ =
    SignalResource'
      { status = pStatus_,
        logicalResourceId = pLogicalResourceId_,
        uniqueId = pUniqueId_,
        stackName = pStackName_
      }

-- | The status of the signal, which is either success or failure. A failure signal causes AWS CloudFormation to immediately fail the stack creation or update.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' SignalResource ResourceSignalStatus
sStatus = Lens.lens (status :: SignalResource -> ResourceSignalStatus) (\s a -> s {status = a} :: SignalResource)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The logical ID of the resource that you want to signal. The logical ID is the name of the resource that given in the template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLogicalResourceId :: Lens.Lens' SignalResource Lude.Text
sLogicalResourceId = Lens.lens (logicalResourceId :: SignalResource -> Lude.Text) (\s a -> s {logicalResourceId = a} :: SignalResource)
{-# DEPRECATED sLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto Scaling groups, specify the instance ID that you are signaling as the unique ID. If you send multiple signals to a single resource (such as signaling a wait condition), each signal requires a different unique ID.
--
-- /Note:/ Consider using 'uniqueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUniqueId :: Lens.Lens' SignalResource Lude.Text
sUniqueId = Lens.lens (uniqueId :: SignalResource -> Lude.Text) (\s a -> s {uniqueId = a} :: SignalResource)
{-# DEPRECATED sUniqueId "Use generic-lens or generic-optics with 'uniqueId' instead." #-}

-- | The stack name or unique stack ID that includes the resource that you want to signal.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackName :: Lens.Lens' SignalResource Lude.Text
sStackName = Lens.lens (stackName :: SignalResource -> Lude.Text) (\s a -> s {stackName = a} :: SignalResource)
{-# DEPRECATED sStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest SignalResource where
  type Rs SignalResource = SignalResourceResponse
  request = Req.postQuery cloudFormationService
  response = Res.receiveNull SignalResourceResponse'

instance Lude.ToHeaders SignalResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SignalResource where
  toPath = Lude.const "/"

instance Lude.ToQuery SignalResource where
  toQuery SignalResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SignalResource" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Status" Lude.=: status,
        "LogicalResourceId" Lude.=: logicalResourceId,
        "UniqueId" Lude.=: uniqueId,
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkSignalResourceResponse' smart constructor.
data SignalResourceResponse = SignalResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SignalResourceResponse' with the minimum fields required to make a request.
mkSignalResourceResponse ::
  SignalResourceResponse
mkSignalResourceResponse = SignalResourceResponse'
