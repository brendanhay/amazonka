{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair, by removing the public key from Amazon EC2.
module Network.AWS.EC2.DeleteKeyPair
  ( -- * Creating a request
    DeleteKeyPair (..),
    mkDeleteKeyPair,

    -- ** Request lenses
    dkpKeyName,
    dkpKeyPairId,
    dkpDryRun,

    -- * Destructuring the response
    DeleteKeyPairResponse (..),
    mkDeleteKeyPairResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { keyName ::
      Lude.Maybe Lude.Text,
    keyPairId :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKeyPair' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'keyName' - The name of the key pair.
-- * 'keyPairId' - The ID of the key pair.
mkDeleteKeyPair ::
  DeleteKeyPair
mkDeleteKeyPair =
  DeleteKeyPair'
    { keyName = Lude.Nothing,
      keyPairId = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyName :: Lens.Lens' DeleteKeyPair (Lude.Maybe Lude.Text)
dkpKeyName = Lens.lens (keyName :: DeleteKeyPair -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: DeleteKeyPair)
{-# DEPRECATED dkpKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairId :: Lens.Lens' DeleteKeyPair (Lude.Maybe Lude.Text)
dkpKeyPairId = Lens.lens (keyPairId :: DeleteKeyPair -> Lude.Maybe Lude.Text) (\s a -> s {keyPairId = a} :: DeleteKeyPair)
{-# DEPRECATED dkpKeyPairId "Use generic-lens or generic-optics with 'keyPairId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpDryRun :: Lens.Lens' DeleteKeyPair (Lude.Maybe Lude.Bool)
dkpDryRun = Lens.lens (dryRun :: DeleteKeyPair -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteKeyPair)
{-# DEPRECATED dkpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteKeyPair where
  type Rs DeleteKeyPair = DeleteKeyPairResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteKeyPairResponse'

instance Lude.ToHeaders DeleteKeyPair where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteKeyPair where
  toQuery DeleteKeyPair' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteKeyPair" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "KeyName" Lude.=: keyName,
        "KeyPairId" Lude.=: keyPairId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKeyPairResponse' with the minimum fields required to make a request.
mkDeleteKeyPairResponse ::
  DeleteKeyPairResponse
mkDeleteKeyPairResponse = DeleteKeyPairResponse'
