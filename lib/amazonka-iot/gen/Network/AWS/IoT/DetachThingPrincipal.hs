{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DetachThingPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified principal from the specified thing. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
module Network.AWS.IoT.DetachThingPrincipal
  ( -- * Creating a request
    DetachThingPrincipal (..),
    mkDetachThingPrincipal,

    -- ** Request lenses
    dtpThingName,
    dtpPrincipal,

    -- * Destructuring the response
    DetachThingPrincipalResponse (..),
    mkDetachThingPrincipalResponse,

    -- ** Response lenses
    dtprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DetachThingPrincipal operation.
--
-- /See:/ 'mkDetachThingPrincipal' smart constructor.
data DetachThingPrincipal = DetachThingPrincipal'
  { thingName ::
      Lude.Text,
    principal :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachThingPrincipal' with the minimum fields required to make a request.
--
-- * 'principal' - If the principal is a certificate, this value must be ARN of the certificate. If the principal is an Amazon Cognito identity, this value must be the ID of the Amazon Cognito identity.
-- * 'thingName' - The name of the thing.
mkDetachThingPrincipal ::
  -- | 'thingName'
  Lude.Text ->
  -- | 'principal'
  Lude.Text ->
  DetachThingPrincipal
mkDetachThingPrincipal pThingName_ pPrincipal_ =
  DetachThingPrincipal'
    { thingName = pThingName_,
      principal = pPrincipal_
    }

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpThingName :: Lens.Lens' DetachThingPrincipal Lude.Text
dtpThingName = Lens.lens (thingName :: DetachThingPrincipal -> Lude.Text) (\s a -> s {thingName = a} :: DetachThingPrincipal)
{-# DEPRECATED dtpThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | If the principal is a certificate, this value must be ARN of the certificate. If the principal is an Amazon Cognito identity, this value must be the ID of the Amazon Cognito identity.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpPrincipal :: Lens.Lens' DetachThingPrincipal Lude.Text
dtpPrincipal = Lens.lens (principal :: DetachThingPrincipal -> Lude.Text) (\s a -> s {principal = a} :: DetachThingPrincipal)
{-# DEPRECATED dtpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

instance Lude.AWSRequest DetachThingPrincipal where
  type Rs DetachThingPrincipal = DetachThingPrincipalResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DetachThingPrincipalResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachThingPrincipal where
  toHeaders DetachThingPrincipal' {..} =
    Lude.mconcat ["x-amzn-principal" Lude.=# principal]

instance Lude.ToPath DetachThingPrincipal where
  toPath DetachThingPrincipal' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/principals"]

instance Lude.ToQuery DetachThingPrincipal where
  toQuery = Lude.const Lude.mempty

-- | The output from the DetachThingPrincipal operation.
--
-- /See:/ 'mkDetachThingPrincipalResponse' smart constructor.
newtype DetachThingPrincipalResponse = DetachThingPrincipalResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachThingPrincipalResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDetachThingPrincipalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachThingPrincipalResponse
mkDetachThingPrincipalResponse pResponseStatus_ =
  DetachThingPrincipalResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprsResponseStatus :: Lens.Lens' DetachThingPrincipalResponse Lude.Int
dtprsResponseStatus = Lens.lens (responseStatus :: DetachThingPrincipalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachThingPrincipalResponse)
{-# DEPRECATED dtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
