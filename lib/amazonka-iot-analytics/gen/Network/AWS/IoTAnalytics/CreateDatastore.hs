{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data store, which is a repository for messages.
module Network.AWS.IoTAnalytics.CreateDatastore
  ( -- * Creating a request
    CreateDatastore (..),
    mkCreateDatastore,

    -- ** Request lenses
    cDatastoreName,
    cRetentionPeriod,
    cDatastoreStorage,
    cTags,

    -- * Destructuring the response
    CreateDatastoreResponse (..),
    mkCreateDatastoreResponse,

    -- ** Response lenses
    cdrsDatastoreARN,
    cdrsDatastoreName,
    cdrsRetentionPeriod,
    cdrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDatastore' smart constructor.
data CreateDatastore = CreateDatastore'
  { -- | The name of the data store.
    datastoreName :: Lude.Text,
    -- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    -- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
    datastoreStorage :: Lude.Maybe DatastoreStorage,
    -- | Metadata which can be used to manage the data store.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatastore' with the minimum fields required to make a request.
--
-- * 'datastoreName' - The name of the data store.
-- * 'retentionPeriod' - How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
-- * 'datastoreStorage' - Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
-- * 'tags' - Metadata which can be used to manage the data store.
mkCreateDatastore ::
  -- | 'datastoreName'
  Lude.Text ->
  CreateDatastore
mkCreateDatastore pDatastoreName_ =
  CreateDatastore'
    { datastoreName = pDatastoreName_,
      retentionPeriod = Lude.Nothing,
      datastoreStorage = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the data store.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDatastoreName :: Lens.Lens' CreateDatastore Lude.Text
cDatastoreName = Lens.lens (datastoreName :: CreateDatastore -> Lude.Text) (\s a -> s {datastoreName = a} :: CreateDatastore)
{-# DEPRECATED cDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

-- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRetentionPeriod :: Lens.Lens' CreateDatastore (Lude.Maybe RetentionPeriod)
cRetentionPeriod = Lens.lens (retentionPeriod :: CreateDatastore -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: CreateDatastore)
{-# DEPRECATED cRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /Note:/ Consider using 'datastoreStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDatastoreStorage :: Lens.Lens' CreateDatastore (Lude.Maybe DatastoreStorage)
cDatastoreStorage = Lens.lens (datastoreStorage :: CreateDatastore -> Lude.Maybe DatastoreStorage) (\s a -> s {datastoreStorage = a} :: CreateDatastore)
{-# DEPRECATED cDatastoreStorage "Use generic-lens or generic-optics with 'datastoreStorage' instead." #-}

-- | Metadata which can be used to manage the data store.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateDatastore (Lude.Maybe (Lude.NonEmpty Tag))
cTags = Lens.lens (tags :: CreateDatastore -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateDatastore)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDatastore where
  type Rs CreateDatastore = CreateDatastoreResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDatastoreResponse'
            Lude.<$> (x Lude..?> "datastoreArn")
            Lude.<*> (x Lude..?> "datastoreName")
            Lude.<*> (x Lude..?> "retentionPeriod")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDatastore where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateDatastore where
  toJSON CreateDatastore' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("datastoreName" Lude..= datastoreName),
            ("retentionPeriod" Lude..=) Lude.<$> retentionPeriod,
            ("datastoreStorage" Lude..=) Lude.<$> datastoreStorage,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDatastore where
  toPath = Lude.const "/datastores"

instance Lude.ToQuery CreateDatastore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDatastoreResponse' smart constructor.
data CreateDatastoreResponse = CreateDatastoreResponse'
  { -- | The ARN of the data store.
    datastoreARN :: Lude.Maybe Lude.Text,
    -- | The name of the data store.
    datastoreName :: Lude.Maybe Lude.Text,
    -- | How long, in days, message data is kept for the data store.
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatastoreResponse' with the minimum fields required to make a request.
--
-- * 'datastoreARN' - The ARN of the data store.
-- * 'datastoreName' - The name of the data store.
-- * 'retentionPeriod' - How long, in days, message data is kept for the data store.
-- * 'responseStatus' - The response status code.
mkCreateDatastoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDatastoreResponse
mkCreateDatastoreResponse pResponseStatus_ =
  CreateDatastoreResponse'
    { datastoreARN = Lude.Nothing,
      datastoreName = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the data store.
--
-- /Note:/ Consider using 'datastoreARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDatastoreARN :: Lens.Lens' CreateDatastoreResponse (Lude.Maybe Lude.Text)
cdrsDatastoreARN = Lens.lens (datastoreARN :: CreateDatastoreResponse -> Lude.Maybe Lude.Text) (\s a -> s {datastoreARN = a} :: CreateDatastoreResponse)
{-# DEPRECATED cdrsDatastoreARN "Use generic-lens or generic-optics with 'datastoreARN' instead." #-}

-- | The name of the data store.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDatastoreName :: Lens.Lens' CreateDatastoreResponse (Lude.Maybe Lude.Text)
cdrsDatastoreName = Lens.lens (datastoreName :: CreateDatastoreResponse -> Lude.Maybe Lude.Text) (\s a -> s {datastoreName = a} :: CreateDatastoreResponse)
{-# DEPRECATED cdrsDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

-- | How long, in days, message data is kept for the data store.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsRetentionPeriod :: Lens.Lens' CreateDatastoreResponse (Lude.Maybe RetentionPeriod)
cdrsRetentionPeriod = Lens.lens (retentionPeriod :: CreateDatastoreResponse -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: CreateDatastoreResponse)
{-# DEPRECATED cdrsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDatastoreResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDatastoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDatastoreResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
