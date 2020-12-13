{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteRegistrationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CA certificate registration code.
module Network.AWS.IoT.DeleteRegistrationCode
  ( -- * Creating a request
    DeleteRegistrationCode (..),
    mkDeleteRegistrationCode,

    -- * Destructuring the response
    DeleteRegistrationCodeResponse (..),
    mkDeleteRegistrationCodeResponse,

    -- ** Response lenses
    drcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeleteRegistrationCode operation.
--
-- /See:/ 'mkDeleteRegistrationCode' smart constructor.
data DeleteRegistrationCode = DeleteRegistrationCode'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRegistrationCode' with the minimum fields required to make a request.
mkDeleteRegistrationCode ::
  DeleteRegistrationCode
mkDeleteRegistrationCode = DeleteRegistrationCode'

instance Lude.AWSRequest DeleteRegistrationCode where
  type Rs DeleteRegistrationCode = DeleteRegistrationCodeResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRegistrationCodeResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRegistrationCode where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRegistrationCode where
  toPath = Lude.const "/registrationcode"

instance Lude.ToQuery DeleteRegistrationCode where
  toQuery = Lude.const Lude.mempty

-- | The output for the DeleteRegistrationCode operation.
--
-- /See:/ 'mkDeleteRegistrationCodeResponse' smart constructor.
newtype DeleteRegistrationCodeResponse = DeleteRegistrationCodeResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRegistrationCodeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRegistrationCodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRegistrationCodeResponse
mkDeleteRegistrationCodeResponse pResponseStatus_ =
  DeleteRegistrationCodeResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DeleteRegistrationCodeResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DeleteRegistrationCodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRegistrationCodeResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
