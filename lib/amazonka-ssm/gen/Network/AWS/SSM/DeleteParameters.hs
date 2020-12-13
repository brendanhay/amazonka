{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a list of parameters.
module Network.AWS.SSM.DeleteParameters
  ( -- * Creating a request
    DeleteParameters (..),
    mkDeleteParameters,

    -- ** Request lenses
    dpNames,

    -- * Destructuring the response
    DeleteParametersResponse (..),
    mkDeleteParametersResponse,

    -- ** Response lenses
    dprsDeletedParameters,
    dprsInvalidParameters,
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteParameters' smart constructor.
newtype DeleteParameters = DeleteParameters'
  { -- | The names of the parameters to delete.
    names :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParameters' with the minimum fields required to make a request.
--
-- * 'names' - The names of the parameters to delete.
mkDeleteParameters ::
  -- | 'names'
  Lude.NonEmpty Lude.Text ->
  DeleteParameters
mkDeleteParameters pNames_ = DeleteParameters' {names = pNames_}

-- | The names of the parameters to delete.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNames :: Lens.Lens' DeleteParameters (Lude.NonEmpty Lude.Text)
dpNames = Lens.lens (names :: DeleteParameters -> Lude.NonEmpty Lude.Text) (\s a -> s {names = a} :: DeleteParameters)
{-# DEPRECATED dpNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Lude.AWSRequest DeleteParameters where
  type Rs DeleteParameters = DeleteParametersResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteParametersResponse'
            Lude.<$> (x Lude..?> "DeletedParameters")
            Lude.<*> (x Lude..?> "InvalidParameters")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteParameters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteParameters where
  toJSON DeleteParameters' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Names" Lude..= names)])

instance Lude.ToPath DeleteParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteParametersResponse' smart constructor.
data DeleteParametersResponse = DeleteParametersResponse'
  { -- | The names of the deleted parameters.
    deletedParameters :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The names of parameters that weren't deleted because the parameters are not valid.
    invalidParameters :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParametersResponse' with the minimum fields required to make a request.
--
-- * 'deletedParameters' - The names of the deleted parameters.
-- * 'invalidParameters' - The names of parameters that weren't deleted because the parameters are not valid.
-- * 'responseStatus' - The response status code.
mkDeleteParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteParametersResponse
mkDeleteParametersResponse pResponseStatus_ =
  DeleteParametersResponse'
    { deletedParameters = Lude.Nothing,
      invalidParameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the deleted parameters.
--
-- /Note:/ Consider using 'deletedParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsDeletedParameters :: Lens.Lens' DeleteParametersResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
dprsDeletedParameters = Lens.lens (deletedParameters :: DeleteParametersResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {deletedParameters = a} :: DeleteParametersResponse)
{-# DEPRECATED dprsDeletedParameters "Use generic-lens or generic-optics with 'deletedParameters' instead." #-}

-- | The names of parameters that weren't deleted because the parameters are not valid.
--
-- /Note:/ Consider using 'invalidParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsInvalidParameters :: Lens.Lens' DeleteParametersResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
dprsInvalidParameters = Lens.lens (invalidParameters :: DeleteParametersResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {invalidParameters = a} :: DeleteParametersResponse)
{-# DEPRECATED dprsInvalidParameters "Use generic-lens or generic-optics with 'invalidParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeleteParametersResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeleteParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteParametersResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
