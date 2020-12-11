{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateDocumentDefaultVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the default version of a document.
module Network.AWS.SSM.UpdateDocumentDefaultVersion
  ( -- * Creating a request
    UpdateDocumentDefaultVersion (..),
    mkUpdateDocumentDefaultVersion,

    -- ** Request lenses
    uddvName,
    uddvDocumentVersion,

    -- * Destructuring the response
    UpdateDocumentDefaultVersionResponse (..),
    mkUpdateDocumentDefaultVersionResponse,

    -- ** Response lenses
    uddvrsDescription,
    uddvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateDocumentDefaultVersion' smart constructor.
data UpdateDocumentDefaultVersion = UpdateDocumentDefaultVersion'
  { name ::
      Lude.Text,
    documentVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentDefaultVersion' with the minimum fields required to make a request.
--
-- * 'documentVersion' - The version of a custom document that you want to set as the default version.
-- * 'name' - The name of a custom document that you want to set as the default version.
mkUpdateDocumentDefaultVersion ::
  -- | 'name'
  Lude.Text ->
  -- | 'documentVersion'
  Lude.Text ->
  UpdateDocumentDefaultVersion
mkUpdateDocumentDefaultVersion pName_ pDocumentVersion_ =
  UpdateDocumentDefaultVersion'
    { name = pName_,
      documentVersion = pDocumentVersion_
    }

-- | The name of a custom document that you want to set as the default version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvName :: Lens.Lens' UpdateDocumentDefaultVersion Lude.Text
uddvName = Lens.lens (name :: UpdateDocumentDefaultVersion -> Lude.Text) (\s a -> s {name = a} :: UpdateDocumentDefaultVersion)
{-# DEPRECATED uddvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of a custom document that you want to set as the default version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvDocumentVersion :: Lens.Lens' UpdateDocumentDefaultVersion Lude.Text
uddvDocumentVersion = Lens.lens (documentVersion :: UpdateDocumentDefaultVersion -> Lude.Text) (\s a -> s {documentVersion = a} :: UpdateDocumentDefaultVersion)
{-# DEPRECATED uddvDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

instance Lude.AWSRequest UpdateDocumentDefaultVersion where
  type
    Rs UpdateDocumentDefaultVersion =
      UpdateDocumentDefaultVersionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDocumentDefaultVersionResponse'
            Lude.<$> (x Lude..?> "Description") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDocumentDefaultVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateDocumentDefaultVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDocumentDefaultVersion where
  toJSON UpdateDocumentDefaultVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("DocumentVersion" Lude..= documentVersion)
          ]
      )

instance Lude.ToPath UpdateDocumentDefaultVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDocumentDefaultVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDocumentDefaultVersionResponse' smart constructor.
data UpdateDocumentDefaultVersionResponse = UpdateDocumentDefaultVersionResponse'
  { description ::
      Lude.Maybe
        DocumentDefaultVersionDescription,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentDefaultVersionResponse' with the minimum fields required to make a request.
--
-- * 'description' - The description of a custom document that you want to set as the default version.
-- * 'responseStatus' - The response status code.
mkUpdateDocumentDefaultVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDocumentDefaultVersionResponse
mkUpdateDocumentDefaultVersionResponse pResponseStatus_ =
  UpdateDocumentDefaultVersionResponse'
    { description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The description of a custom document that you want to set as the default version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvrsDescription :: Lens.Lens' UpdateDocumentDefaultVersionResponse (Lude.Maybe DocumentDefaultVersionDescription)
uddvrsDescription = Lens.lens (description :: UpdateDocumentDefaultVersionResponse -> Lude.Maybe DocumentDefaultVersionDescription) (\s a -> s {description = a} :: UpdateDocumentDefaultVersionResponse)
{-# DEPRECATED uddvrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvrsResponseStatus :: Lens.Lens' UpdateDocumentDefaultVersionResponse Lude.Int
uddvrsResponseStatus = Lens.lens (responseStatus :: UpdateDocumentDefaultVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDocumentDefaultVersionResponse)
{-# DEPRECATED uddvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
