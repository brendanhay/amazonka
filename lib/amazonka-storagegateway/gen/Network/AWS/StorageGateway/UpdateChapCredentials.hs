{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target. By default, a gateway does not have CHAP enabled; however, for added security, you might use it. This operation is supported in the volume and tape gateway types.
--
-- /Important:/ When you update CHAP credentials, all existing connections on the target are closed and initiators must reconnect with the new credentials.
module Network.AWS.StorageGateway.UpdateChapCredentials
  ( -- * Creating a request
    UpdateChapCredentials (..),
    mkUpdateChapCredentials,

    -- ** Request lenses
    uccSecretToAuthenticateTarget,
    uccTargetARN,
    uccSecretToAuthenticateInitiator,
    uccInitiatorName,

    -- * Destructuring the response
    UpdateChapCredentialsResponse (..),
    mkUpdateChapCredentialsResponse,

    -- ** Response lenses
    uccrsTargetARN,
    uccrsInitiatorName,
    uccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'UpdateChapCredentialsInput$InitiatorName'
--
--
--     * 'UpdateChapCredentialsInput$SecretToAuthenticateInitiator'
--
--
--     * 'UpdateChapCredentialsInput$SecretToAuthenticateTarget'
--
--
--     * 'UpdateChapCredentialsInput$TargetARN'
--
--
--
-- /See:/ 'mkUpdateChapCredentials' smart constructor.
data UpdateChapCredentials = UpdateChapCredentials'
  { secretToAuthenticateTarget ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    targetARN :: Lude.Text,
    secretToAuthenticateInitiator ::
      Lude.Sensitive Lude.Text,
    initiatorName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChapCredentials' with the minimum fields required to make a request.
--
-- * 'initiatorName' - The iSCSI initiator that connects to the target.
-- * 'secretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
-- * 'secretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return the TargetARN for specified VolumeARN.
mkUpdateChapCredentials ::
  -- | 'targetARN'
  Lude.Text ->
  -- | 'secretToAuthenticateInitiator'
  Lude.Sensitive Lude.Text ->
  -- | 'initiatorName'
  Lude.Text ->
  UpdateChapCredentials
mkUpdateChapCredentials
  pTargetARN_
  pSecretToAuthenticateInitiator_
  pInitiatorName_ =
    UpdateChapCredentials'
      { secretToAuthenticateTarget = Lude.Nothing,
        targetARN = pTargetARN_,
        secretToAuthenticateInitiator = pSecretToAuthenticateInitiator_,
        initiatorName = pInitiatorName_
      }

-- | The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
--
-- /Note:/ Consider using 'secretToAuthenticateTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccSecretToAuthenticateTarget :: Lens.Lens' UpdateChapCredentials (Lude.Maybe (Lude.Sensitive Lude.Text))
uccSecretToAuthenticateTarget = Lens.lens (secretToAuthenticateTarget :: UpdateChapCredentials -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretToAuthenticateTarget = a} :: UpdateChapCredentials)
{-# DEPRECATED uccSecretToAuthenticateTarget "Use generic-lens or generic-optics with 'secretToAuthenticateTarget' instead." #-}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return the TargetARN for specified VolumeARN.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccTargetARN :: Lens.Lens' UpdateChapCredentials Lude.Text
uccTargetARN = Lens.lens (targetARN :: UpdateChapCredentials -> Lude.Text) (\s a -> s {targetARN = a} :: UpdateChapCredentials)
{-# DEPRECATED uccTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
-- /Note:/ Consider using 'secretToAuthenticateInitiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccSecretToAuthenticateInitiator :: Lens.Lens' UpdateChapCredentials (Lude.Sensitive Lude.Text)
uccSecretToAuthenticateInitiator = Lens.lens (secretToAuthenticateInitiator :: UpdateChapCredentials -> Lude.Sensitive Lude.Text) (\s a -> s {secretToAuthenticateInitiator = a} :: UpdateChapCredentials)
{-# DEPRECATED uccSecretToAuthenticateInitiator "Use generic-lens or generic-optics with 'secretToAuthenticateInitiator' instead." #-}

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccInitiatorName :: Lens.Lens' UpdateChapCredentials Lude.Text
uccInitiatorName = Lens.lens (initiatorName :: UpdateChapCredentials -> Lude.Text) (\s a -> s {initiatorName = a} :: UpdateChapCredentials)
{-# DEPRECATED uccInitiatorName "Use generic-lens or generic-optics with 'initiatorName' instead." #-}

instance Lude.AWSRequest UpdateChapCredentials where
  type Rs UpdateChapCredentials = UpdateChapCredentialsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateChapCredentialsResponse'
            Lude.<$> (x Lude..?> "TargetARN")
            Lude.<*> (x Lude..?> "InitiatorName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateChapCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateChapCredentials" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateChapCredentials where
  toJSON UpdateChapCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecretToAuthenticateTarget" Lude..=)
              Lude.<$> secretToAuthenticateTarget,
            Lude.Just ("TargetARN" Lude..= targetARN),
            Lude.Just
              ( "SecretToAuthenticateInitiator"
                  Lude..= secretToAuthenticateInitiator
              ),
            Lude.Just ("InitiatorName" Lude..= initiatorName)
          ]
      )

instance Lude.ToPath UpdateChapCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateChapCredentials where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkUpdateChapCredentialsResponse' smart constructor.
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse'
  { targetARN ::
      Lude.Maybe Lude.Text,
    initiatorName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateChapCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'initiatorName' - The iSCSI initiator that connects to the target. This is the same initiator name specified in the request.
-- * 'responseStatus' - The response status code.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the target. This is the same target specified in the request.
mkUpdateChapCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateChapCredentialsResponse
mkUpdateChapCredentialsResponse pResponseStatus_ =
  UpdateChapCredentialsResponse'
    { targetARN = Lude.Nothing,
      initiatorName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the target. This is the same target specified in the request.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrsTargetARN :: Lens.Lens' UpdateChapCredentialsResponse (Lude.Maybe Lude.Text)
uccrsTargetARN = Lens.lens (targetARN :: UpdateChapCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: UpdateChapCredentialsResponse)
{-# DEPRECATED uccrsTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The iSCSI initiator that connects to the target. This is the same initiator name specified in the request.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrsInitiatorName :: Lens.Lens' UpdateChapCredentialsResponse (Lude.Maybe Lude.Text)
uccrsInitiatorName = Lens.lens (initiatorName :: UpdateChapCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {initiatorName = a} :: UpdateChapCredentialsResponse)
{-# DEPRECATED uccrsInitiatorName "Use generic-lens or generic-optics with 'initiatorName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrsResponseStatus :: Lens.Lens' UpdateChapCredentialsResponse Lude.Int
uccrsResponseStatus = Lens.lens (responseStatus :: UpdateChapCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateChapCredentialsResponse)
{-# DEPRECATED uccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
