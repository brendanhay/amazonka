{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteSchemaVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove versions from the specified schema. A version number or range may be supplied. If the compatibility mode forbids deleting of a version that is necessary, such as BACKWARDS_FULL, an error is returned. Calling the @GetSchemaVersions@ API after this call will list the status of the deleted versions.
--
-- When the range of version numbers contain check pointed version, the API will return a 409 conflict and will not proceed with the deletion. You have to remove the checkpoint first using the @DeleteSchemaCheckpoint@ API before using this API.
-- You cannot use the @DeleteSchemaVersions@ API to delete the first schema version in the schema set. The first schema version can only be deleted by the @DeleteSchema@ API. This operation will also delete the attached @SchemaVersionMetadata@ under the schema versions. Hard deletes will be enforced on the database.
-- If the compatibility mode forbids deleting of a version that is necessary, such as BACKWARDS_FULL, an error is returned.
module Network.AWS.Glue.DeleteSchemaVersions
  ( -- * Creating a request
    DeleteSchemaVersions (..),
    mkDeleteSchemaVersions,

    -- ** Request lenses
    dsvSchemaId,
    dsvVersions,

    -- * Destructuring the response
    DeleteSchemaVersionsResponse (..),
    mkDeleteSchemaVersionsResponse,

    -- ** Response lenses
    dsvrsSchemaVersionErrors,
    dsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSchemaVersions' smart constructor.
data DeleteSchemaVersions = DeleteSchemaVersions'
  { schemaId ::
      SchemaId,
    versions :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSchemaVersions' with the minimum fields required to make a request.
--
-- * 'schemaId' - This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
-- * 'versions' - A version range may be supplied which may be of the format:
--
--
--     * a single version number, 5
--
--
--     * a range, 5-8 : deletes versions 5, 6, 7, 8
mkDeleteSchemaVersions ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'versions'
  Lude.Text ->
  DeleteSchemaVersions
mkDeleteSchemaVersions pSchemaId_ pVersions_ =
  DeleteSchemaVersions'
    { schemaId = pSchemaId_,
      versions = pVersions_
    }

-- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvSchemaId :: Lens.Lens' DeleteSchemaVersions SchemaId
dsvSchemaId = Lens.lens (schemaId :: DeleteSchemaVersions -> SchemaId) (\s a -> s {schemaId = a} :: DeleteSchemaVersions)
{-# DEPRECATED dsvSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | A version range may be supplied which may be of the format:
--
--
--     * a single version number, 5
--
--
--     * a range, 5-8 : deletes versions 5, 6, 7, 8
--
--
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvVersions :: Lens.Lens' DeleteSchemaVersions Lude.Text
dsvVersions = Lens.lens (versions :: DeleteSchemaVersions -> Lude.Text) (\s a -> s {versions = a} :: DeleteSchemaVersions)
{-# DEPRECATED dsvVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

instance Lude.AWSRequest DeleteSchemaVersions where
  type Rs DeleteSchemaVersions = DeleteSchemaVersionsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSchemaVersionsResponse'
            Lude.<$> (x Lude..?> "SchemaVersionErrors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSchemaVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteSchemaVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSchemaVersions where
  toJSON DeleteSchemaVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaId" Lude..= schemaId),
            Lude.Just ("Versions" Lude..= versions)
          ]
      )

instance Lude.ToPath DeleteSchemaVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSchemaVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSchemaVersionsResponse' smart constructor.
data DeleteSchemaVersionsResponse = DeleteSchemaVersionsResponse'
  { schemaVersionErrors ::
      Lude.Maybe
        [SchemaVersionErrorItem],
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

-- | Creates a value of 'DeleteSchemaVersionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'schemaVersionErrors' - A list of @SchemaVersionErrorItem@ objects, each containing an error and schema version.
mkDeleteSchemaVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSchemaVersionsResponse
mkDeleteSchemaVersionsResponse pResponseStatus_ =
  DeleteSchemaVersionsResponse'
    { schemaVersionErrors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @SchemaVersionErrorItem@ objects, each containing an error and schema version.
--
-- /Note:/ Consider using 'schemaVersionErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvrsSchemaVersionErrors :: Lens.Lens' DeleteSchemaVersionsResponse (Lude.Maybe [SchemaVersionErrorItem])
dsvrsSchemaVersionErrors = Lens.lens (schemaVersionErrors :: DeleteSchemaVersionsResponse -> Lude.Maybe [SchemaVersionErrorItem]) (\s a -> s {schemaVersionErrors = a} :: DeleteSchemaVersionsResponse)
{-# DEPRECATED dsvrsSchemaVersionErrors "Use generic-lens or generic-optics with 'schemaVersionErrors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvrsResponseStatus :: Lens.Lens' DeleteSchemaVersionsResponse Lude.Int
dsvrsResponseStatus = Lens.lens (responseStatus :: DeleteSchemaVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSchemaVersionsResponse)
{-# DEPRECATED dsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
