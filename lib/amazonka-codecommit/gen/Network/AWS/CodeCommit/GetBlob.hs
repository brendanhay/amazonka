{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetBlob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded content of an individual blob in a repository.
module Network.AWS.CodeCommit.GetBlob
  ( -- * Creating a request
    GetBlob (..),
    mkGetBlob,

    -- ** Request lenses
    gBlobId,
    gRepositoryName,

    -- * Destructuring the response
    GetBlobResponse (..),
    mkGetBlobResponse,

    -- ** Response lenses
    gbrsContent,
    gbrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a get blob operation.
--
-- /See:/ 'mkGetBlob' smart constructor.
data GetBlob = GetBlob'
  { -- | The ID of the blob, which is its SHA-1 pointer.
    blobId :: Lude.Text,
    -- | The name of the repository that contains the blob.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBlob' with the minimum fields required to make a request.
--
-- * 'blobId' - The ID of the blob, which is its SHA-1 pointer.
-- * 'repositoryName' - The name of the repository that contains the blob.
mkGetBlob ::
  -- | 'blobId'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  GetBlob
mkGetBlob pBlobId_ pRepositoryName_ =
  GetBlob' {blobId = pBlobId_, repositoryName = pRepositoryName_}

-- | The ID of the blob, which is its SHA-1 pointer.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBlobId :: Lens.Lens' GetBlob Lude.Text
gBlobId = Lens.lens (blobId :: GetBlob -> Lude.Text) (\s a -> s {blobId = a} :: GetBlob)
{-# DEPRECATED gBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The name of the repository that contains the blob.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gRepositoryName :: Lens.Lens' GetBlob Lude.Text
gRepositoryName = Lens.lens (repositoryName :: GetBlob -> Lude.Text) (\s a -> s {repositoryName = a} :: GetBlob)
{-# DEPRECATED gRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetBlob where
  type Rs GetBlob = GetBlobResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBlobResponse'
            Lude.<$> (x Lude..:> "content") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBlob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetBlob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBlob where
  toJSON GetBlob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("blobId" Lude..= blobId),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath GetBlob where
  toPath = Lude.const "/"

instance Lude.ToQuery GetBlob where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a get blob operation.
--
-- /See:/ 'mkGetBlobResponse' smart constructor.
data GetBlobResponse = GetBlobResponse'
  { -- | The content of the blob, usually a file.
    content :: Lude.Base64,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBlobResponse' with the minimum fields required to make a request.
--
-- * 'content' - The content of the blob, usually a file.
-- * 'responseStatus' - The response status code.
mkGetBlobResponse ::
  -- | 'content'
  Lude.Base64 ->
  -- | 'responseStatus'
  Lude.Int ->
  GetBlobResponse
mkGetBlobResponse pContent_ pResponseStatus_ =
  GetBlobResponse'
    { content = pContent_,
      responseStatus = pResponseStatus_
    }

-- | The content of the blob, usually a file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsContent :: Lens.Lens' GetBlobResponse Lude.Base64
gbrsContent = Lens.lens (content :: GetBlobResponse -> Lude.Base64) (\s a -> s {content = a} :: GetBlobResponse)
{-# DEPRECATED gbrsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsResponseStatus :: Lens.Lens' GetBlobResponse Lude.Int
gbrsResponseStatus = Lens.lens (responseStatus :: GetBlobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBlobResponse)
{-# DEPRECATED gbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
