{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AttachThingPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified principal to the specified thing. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
module Network.AWS.IoT.AttachThingPrincipal
  ( -- * Creating a request
    AttachThingPrincipal (..),
    mkAttachThingPrincipal,

    -- ** Request lenses
    atpPrincipal,
    atpThingName,

    -- * Destructuring the response
    AttachThingPrincipalResponse (..),
    mkAttachThingPrincipalResponse,

    -- ** Response lenses
    atprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the AttachThingPrincipal operation.
--
-- /See:/ 'mkAttachThingPrincipal' smart constructor.
data AttachThingPrincipal = AttachThingPrincipal'
  { -- | The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
    principal :: Lude.Text,
    -- | The name of the thing.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachThingPrincipal' with the minimum fields required to make a request.
--
-- * 'principal' - The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
-- * 'thingName' - The name of the thing.
mkAttachThingPrincipal ::
  -- | 'principal'
  Lude.Text ->
  -- | 'thingName'
  Lude.Text ->
  AttachThingPrincipal
mkAttachThingPrincipal pPrincipal_ pThingName_ =
  AttachThingPrincipal'
    { principal = pPrincipal_,
      thingName = pThingName_
    }

-- | The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpPrincipal :: Lens.Lens' AttachThingPrincipal Lude.Text
atpPrincipal = Lens.lens (principal :: AttachThingPrincipal -> Lude.Text) (\s a -> s {principal = a} :: AttachThingPrincipal)
{-# DEPRECATED atpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpThingName :: Lens.Lens' AttachThingPrincipal Lude.Text
atpThingName = Lens.lens (thingName :: AttachThingPrincipal -> Lude.Text) (\s a -> s {thingName = a} :: AttachThingPrincipal)
{-# DEPRECATED atpThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest AttachThingPrincipal where
  type Rs AttachThingPrincipal = AttachThingPrincipalResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AttachThingPrincipalResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachThingPrincipal where
  toHeaders AttachThingPrincipal' {..} =
    Lude.mconcat ["x-amzn-principal" Lude.=# principal]

instance Lude.ToJSON AttachThingPrincipal where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath AttachThingPrincipal where
  toPath AttachThingPrincipal' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/principals"]

instance Lude.ToQuery AttachThingPrincipal where
  toQuery = Lude.const Lude.mempty

-- | The output from the AttachThingPrincipal operation.
--
-- /See:/ 'mkAttachThingPrincipalResponse' smart constructor.
newtype AttachThingPrincipalResponse = AttachThingPrincipalResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachThingPrincipalResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAttachThingPrincipalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachThingPrincipalResponse
mkAttachThingPrincipalResponse pResponseStatus_ =
  AttachThingPrincipalResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atprsResponseStatus :: Lens.Lens' AttachThingPrincipalResponse Lude.Int
atprsResponseStatus = Lens.lens (responseStatus :: AttachThingPrincipalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachThingPrincipalResponse)
{-# DEPRECATED atprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
