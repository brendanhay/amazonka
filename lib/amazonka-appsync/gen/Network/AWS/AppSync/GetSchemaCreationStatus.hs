{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetSchemaCreationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of a schema creation operation.
module Network.AWS.AppSync.GetSchemaCreationStatus
  ( -- * Creating a request
    GetSchemaCreationStatus (..),
    mkGetSchemaCreationStatus,

    -- ** Request lenses
    gscsApiId,

    -- * Destructuring the response
    GetSchemaCreationStatusResponse (..),
    mkGetSchemaCreationStatusResponse,

    -- ** Response lenses
    gscsrsStatus,
    gscsrsDetails,
    gscsrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSchemaCreationStatus' smart constructor.
newtype GetSchemaCreationStatus = GetSchemaCreationStatus'
  { apiId ::
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

-- | Creates a value of 'GetSchemaCreationStatus' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
mkGetSchemaCreationStatus ::
  -- | 'apiId'
  Lude.Text ->
  GetSchemaCreationStatus
mkGetSchemaCreationStatus pApiId_ =
  GetSchemaCreationStatus' {apiId = pApiId_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsApiId :: Lens.Lens' GetSchemaCreationStatus Lude.Text
gscsApiId = Lens.lens (apiId :: GetSchemaCreationStatus -> Lude.Text) (\s a -> s {apiId = a} :: GetSchemaCreationStatus)
{-# DEPRECATED gscsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest GetSchemaCreationStatus where
  type Rs GetSchemaCreationStatus = GetSchemaCreationStatusResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSchemaCreationStatusResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "details")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSchemaCreationStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSchemaCreationStatus where
  toPath GetSchemaCreationStatus' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/schemacreation"]

instance Lude.ToQuery GetSchemaCreationStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSchemaCreationStatusResponse' smart constructor.
data GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse'
  { status ::
      Lude.Maybe SchemaStatus,
    details ::
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

-- | Creates a value of 'GetSchemaCreationStatusResponse' with the minimum fields required to make a request.
--
-- * 'details' - Detailed information about the status of the schema creation operation.
-- * 'responseStatus' - The response status code.
-- * 'status' - The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
mkGetSchemaCreationStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSchemaCreationStatusResponse
mkGetSchemaCreationStatusResponse pResponseStatus_ =
  GetSchemaCreationStatusResponse'
    { status = Lude.Nothing,
      details = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsStatus :: Lens.Lens' GetSchemaCreationStatusResponse (Lude.Maybe SchemaStatus)
gscsrsStatus = Lens.lens (status :: GetSchemaCreationStatusResponse -> Lude.Maybe SchemaStatus) (\s a -> s {status = a} :: GetSchemaCreationStatusResponse)
{-# DEPRECATED gscsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Detailed information about the status of the schema creation operation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsDetails :: Lens.Lens' GetSchemaCreationStatusResponse (Lude.Maybe Lude.Text)
gscsrsDetails = Lens.lens (details :: GetSchemaCreationStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: GetSchemaCreationStatusResponse)
{-# DEPRECATED gscsrsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsResponseStatus :: Lens.Lens' GetSchemaCreationStatusResponse Lude.Int
gscsrsResponseStatus = Lens.lens (responseStatus :: GetSchemaCreationStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSchemaCreationStatusResponse)
{-# DEPRECATED gscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
