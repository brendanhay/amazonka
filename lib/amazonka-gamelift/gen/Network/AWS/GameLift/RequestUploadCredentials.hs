{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.RequestUploadCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fresh set of credentials for use when uploading a new set of game build files to Amazon GameLift's Amazon S3. This is done as part of the build creation process; see 'CreateBuild' .
--
-- To request new credentials, specify the build ID as returned with an initial @CreateBuild@ request. If successful, a new set of credentials are returned, along with the S3 storage location associated with the build ID.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in S3>
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
module Network.AWS.GameLift.RequestUploadCredentials
  ( -- * Creating a request
    RequestUploadCredentials (..),
    mkRequestUploadCredentials,

    -- ** Request lenses
    rucBuildId,

    -- * Destructuring the response
    RequestUploadCredentialsResponse (..),
    mkRequestUploadCredentialsResponse,

    -- ** Response lenses
    rucrsStorageLocation,
    rucrsUploadCredentials,
    rucrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkRequestUploadCredentials' smart constructor.
newtype RequestUploadCredentials = RequestUploadCredentials'
  { buildId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestUploadCredentials' with the minimum fields required to make a request.
--
-- * 'buildId' - A unique identifier for a build to get credentials for. You can use either the build ID or ARN value.
mkRequestUploadCredentials ::
  -- | 'buildId'
  Lude.Text ->
  RequestUploadCredentials
mkRequestUploadCredentials pBuildId_ =
  RequestUploadCredentials' {buildId = pBuildId_}

-- | A unique identifier for a build to get credentials for. You can use either the build ID or ARN value.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucBuildId :: Lens.Lens' RequestUploadCredentials Lude.Text
rucBuildId = Lens.lens (buildId :: RequestUploadCredentials -> Lude.Text) (\s a -> s {buildId = a} :: RequestUploadCredentials)
{-# DEPRECATED rucBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

instance Lude.AWSRequest RequestUploadCredentials where
  type Rs RequestUploadCredentials = RequestUploadCredentialsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          RequestUploadCredentialsResponse'
            Lude.<$> (x Lude..?> "StorageLocation")
            Lude.<*> (x Lude..?> "UploadCredentials")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RequestUploadCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.RequestUploadCredentials" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RequestUploadCredentials where
  toJSON RequestUploadCredentials' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BuildId" Lude..= buildId)])

instance Lude.ToPath RequestUploadCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery RequestUploadCredentials where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkRequestUploadCredentialsResponse' smart constructor.
data RequestUploadCredentialsResponse = RequestUploadCredentialsResponse'
  { storageLocation ::
      Lude.Maybe S3Location,
    uploadCredentials ::
      Lude.Maybe AWSCredentials,
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

-- | Creates a value of 'RequestUploadCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'storageLocation' - Amazon S3 path and key, identifying where the game build files are stored.
-- * 'uploadCredentials' - AWS credentials required when uploading a game build to the storage location. These credentials have a limited lifespan and are valid only for the build they were issued for.
mkRequestUploadCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RequestUploadCredentialsResponse
mkRequestUploadCredentialsResponse pResponseStatus_ =
  RequestUploadCredentialsResponse'
    { storageLocation = Lude.Nothing,
      uploadCredentials = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Amazon S3 path and key, identifying where the game build files are stored.
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucrsStorageLocation :: Lens.Lens' RequestUploadCredentialsResponse (Lude.Maybe S3Location)
rucrsStorageLocation = Lens.lens (storageLocation :: RequestUploadCredentialsResponse -> Lude.Maybe S3Location) (\s a -> s {storageLocation = a} :: RequestUploadCredentialsResponse)
{-# DEPRECATED rucrsStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | AWS credentials required when uploading a game build to the storage location. These credentials have a limited lifespan and are valid only for the build they were issued for.
--
-- /Note:/ Consider using 'uploadCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucrsUploadCredentials :: Lens.Lens' RequestUploadCredentialsResponse (Lude.Maybe AWSCredentials)
rucrsUploadCredentials = Lens.lens (uploadCredentials :: RequestUploadCredentialsResponse -> Lude.Maybe AWSCredentials) (\s a -> s {uploadCredentials = a} :: RequestUploadCredentialsResponse)
{-# DEPRECATED rucrsUploadCredentials "Use generic-lens or generic-optics with 'uploadCredentials' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucrsResponseStatus :: Lens.Lens' RequestUploadCredentialsResponse Lude.Int
rucrsResponseStatus = Lens.lens (responseStatus :: RequestUploadCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RequestUploadCredentialsResponse)
{-# DEPRECATED rucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
