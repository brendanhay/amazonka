{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates an Amazon Connect instance with all the supported channels enabled. It does not attach any storage (such as Amazon S3, or Kinesis) or allow for any configurations on features such as Contact Lens for Amazon Connect.
module Network.AWS.Connect.CreateInstance
  ( -- * Creating a request
    CreateInstance (..),
    mkCreateInstance,

    -- ** Request lenses
    ciDirectoryId,
    ciClientToken,
    ciInstanceAlias,
    ciIdentityManagementType,
    ciInboundCallsEnabled,
    ciOutboundCallsEnabled,

    -- * Destructuring the response
    CreateInstanceResponse (..),
    mkCreateInstanceResponse,

    -- ** Response lenses
    cirsARN,
    cirsId,
    cirsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { directoryId ::
      Lude.Maybe Lude.Text,
    clientToken :: Lude.Maybe Lude.Text,
    instanceAlias :: Lude.Maybe (Lude.Sensitive Lude.Text),
    identityManagementType :: DirectoryType,
    inboundCallsEnabled :: Lude.Bool,
    outboundCallsEnabled :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstance' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token.
-- * 'directoryId' - The identifier for the directory.
-- * 'identityManagementType' - The type of identity management for your Amazon Connect users.
-- * 'inboundCallsEnabled' - Whether your contact center handles incoming contacts.
-- * 'instanceAlias' - The name for your instance.
-- * 'outboundCallsEnabled' - Whether your contact center allows outbound calls.
mkCreateInstance ::
  -- | 'identityManagementType'
  DirectoryType ->
  -- | 'inboundCallsEnabled'
  Lude.Bool ->
  -- | 'outboundCallsEnabled'
  Lude.Bool ->
  CreateInstance
mkCreateInstance
  pIdentityManagementType_
  pInboundCallsEnabled_
  pOutboundCallsEnabled_ =
    CreateInstance'
      { directoryId = Lude.Nothing,
        clientToken = Lude.Nothing,
        instanceAlias = Lude.Nothing,
        identityManagementType = pIdentityManagementType_,
        inboundCallsEnabled = pInboundCallsEnabled_,
        outboundCallsEnabled = pOutboundCallsEnabled_
      }

-- | The identifier for the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDirectoryId :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciDirectoryId = Lens.lens (directoryId :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: CreateInstance)
{-# DEPRECATED ciDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciClientToken :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciClientToken = Lens.lens (clientToken :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateInstance)
{-# DEPRECATED ciClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name for your instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceAlias :: Lens.Lens' CreateInstance (Lude.Maybe (Lude.Sensitive Lude.Text))
ciInstanceAlias = Lens.lens (instanceAlias :: CreateInstance -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {instanceAlias = a} :: CreateInstance)
{-# DEPRECATED ciInstanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead." #-}

-- | The type of identity management for your Amazon Connect users.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciIdentityManagementType :: Lens.Lens' CreateInstance DirectoryType
ciIdentityManagementType = Lens.lens (identityManagementType :: CreateInstance -> DirectoryType) (\s a -> s {identityManagementType = a} :: CreateInstance)
{-# DEPRECATED ciIdentityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead." #-}

-- | Whether your contact center handles incoming contacts.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInboundCallsEnabled :: Lens.Lens' CreateInstance Lude.Bool
ciInboundCallsEnabled = Lens.lens (inboundCallsEnabled :: CreateInstance -> Lude.Bool) (\s a -> s {inboundCallsEnabled = a} :: CreateInstance)
{-# DEPRECATED ciInboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead." #-}

-- | Whether your contact center allows outbound calls.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOutboundCallsEnabled :: Lens.Lens' CreateInstance Lude.Bool
ciOutboundCallsEnabled = Lens.lens (outboundCallsEnabled :: CreateInstance -> Lude.Bool) (\s a -> s {outboundCallsEnabled = a} :: CreateInstance)
{-# DEPRECATED ciOutboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead." #-}

instance Lude.AWSRequest CreateInstance where
  type Rs CreateInstance = CreateInstanceResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInstance where
  toJSON CreateInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("InstanceAlias" Lude..=) Lude.<$> instanceAlias,
            Lude.Just
              ("IdentityManagementType" Lude..= identityManagementType),
            Lude.Just ("InboundCallsEnabled" Lude..= inboundCallsEnabled),
            Lude.Just ("OutboundCallsEnabled" Lude..= outboundCallsEnabled)
          ]
      )

instance Lude.ToPath CreateInstance where
  toPath = Lude.const "/instance"

instance Lude.ToQuery CreateInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance.
-- * 'id' - The identifier for the instance.
-- * 'responseStatus' - The response status code.
mkCreateInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstanceResponse
mkCreateInstanceResponse pResponseStatus_ =
  CreateInstanceResponse'
    { arn = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsARN :: Lens.Lens' CreateInstanceResponse (Lude.Maybe Lude.Text)
cirsARN = Lens.lens (arn :: CreateInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateInstanceResponse)
{-# DEPRECATED cirsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The identifier for the instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsId :: Lens.Lens' CreateInstanceResponse (Lude.Maybe Lude.Text)
cirsId = Lens.lens (id :: CreateInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateInstanceResponse)
{-# DEPRECATED cirsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateInstanceResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstanceResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
