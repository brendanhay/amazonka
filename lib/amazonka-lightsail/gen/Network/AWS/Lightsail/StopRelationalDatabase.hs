{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StopRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific database that is currently running in Amazon Lightsail.
--
-- The @stop relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StopRelationalDatabase
  ( -- * Creating a request
    StopRelationalDatabase (..),
    mkStopRelationalDatabase,

    -- ** Request lenses
    srdRelationalDatabaseSnapshotName,
    srdRelationalDatabaseName,

    -- * Destructuring the response
    StopRelationalDatabaseResponse (..),
    mkStopRelationalDatabaseResponse,

    -- ** Response lenses
    storsOperations,
    storsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopRelationalDatabase' smart constructor.
data StopRelationalDatabase = StopRelationalDatabase'
  { relationalDatabaseSnapshotName ::
      Lude.Maybe Lude.Text,
    relationalDatabaseName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopRelationalDatabase' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseName' - The name of your database to stop.
-- * 'relationalDatabaseSnapshotName' - The name of your new database snapshot to be created before stopping your database.
mkStopRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  StopRelationalDatabase
mkStopRelationalDatabase pRelationalDatabaseName_ =
  StopRelationalDatabase'
    { relationalDatabaseSnapshotName =
        Lude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The name of your new database snapshot to be created before stopping your database.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdRelationalDatabaseSnapshotName :: Lens.Lens' StopRelationalDatabase (Lude.Maybe Lude.Text)
srdRelationalDatabaseSnapshotName = Lens.lens (relationalDatabaseSnapshotName :: StopRelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {relationalDatabaseSnapshotName = a} :: StopRelationalDatabase)
{-# DEPRECATED srdRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

-- | The name of your database to stop.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdRelationalDatabaseName :: Lens.Lens' StopRelationalDatabase Lude.Text
srdRelationalDatabaseName = Lens.lens (relationalDatabaseName :: StopRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: StopRelationalDatabase)
{-# DEPRECATED srdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest StopRelationalDatabase where
  type Rs StopRelationalDatabase = StopRelationalDatabaseResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopRelationalDatabaseResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopRelationalDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.StopRelationalDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopRelationalDatabase where
  toJSON StopRelationalDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("relationalDatabaseSnapshotName" Lude..=)
              Lude.<$> relationalDatabaseSnapshotName,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath StopRelationalDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery StopRelationalDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopRelationalDatabaseResponse' smart constructor.
data StopRelationalDatabaseResponse = StopRelationalDatabaseResponse'
  { operations ::
      Lude.Maybe [Operation],
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

-- | Creates a value of 'StopRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkStopRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopRelationalDatabaseResponse
mkStopRelationalDatabaseResponse pResponseStatus_ =
  StopRelationalDatabaseResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsOperations :: Lens.Lens' StopRelationalDatabaseResponse (Lude.Maybe [Operation])
storsOperations = Lens.lens (operations :: StopRelationalDatabaseResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: StopRelationalDatabaseResponse)
{-# DEPRECATED storsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsResponseStatus :: Lens.Lens' StopRelationalDatabaseResponse Lude.Int
storsResponseStatus = Lens.lens (responseStatus :: StopRelationalDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopRelationalDatabaseResponse)
{-# DEPRECATED storsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
