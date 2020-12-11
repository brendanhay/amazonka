{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database snapshot in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
  ( -- * Creating a request
    GetRelationalDatabaseSnapshot (..),
    mkGetRelationalDatabaseSnapshot,

    -- ** Request lenses
    grdsRelationalDatabaseSnapshotName,

    -- * Destructuring the response
    GetRelationalDatabaseSnapshotResponse (..),
    mkGetRelationalDatabaseSnapshotResponse,

    -- ** Response lenses
    getrsRelationalDatabaseSnapshot,
    getrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseSnapshot' smart constructor.
newtype GetRelationalDatabaseSnapshot = GetRelationalDatabaseSnapshot'
  { relationalDatabaseSnapshotName ::
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

-- | Creates a value of 'GetRelationalDatabaseSnapshot' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseSnapshotName' - The name of the database snapshot for which to get information.
mkGetRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseSnapshotName'
  Lude.Text ->
  GetRelationalDatabaseSnapshot
mkGetRelationalDatabaseSnapshot pRelationalDatabaseSnapshotName_ =
  GetRelationalDatabaseSnapshot'
    { relationalDatabaseSnapshotName =
        pRelationalDatabaseSnapshotName_
    }

-- | The name of the database snapshot for which to get information.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsRelationalDatabaseSnapshotName :: Lens.Lens' GetRelationalDatabaseSnapshot Lude.Text
grdsRelationalDatabaseSnapshotName = Lens.lens (relationalDatabaseSnapshotName :: GetRelationalDatabaseSnapshot -> Lude.Text) (\s a -> s {relationalDatabaseSnapshotName = a} :: GetRelationalDatabaseSnapshot)
{-# DEPRECATED grdsRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

instance Lude.AWSRequest GetRelationalDatabaseSnapshot where
  type
    Rs GetRelationalDatabaseSnapshot =
      GetRelationalDatabaseSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseSnapshotResponse'
            Lude.<$> (x Lude..?> "relationalDatabaseSnapshot")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseSnapshot" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseSnapshot where
  toJSON GetRelationalDatabaseSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "relationalDatabaseSnapshotName"
                  Lude..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Lude.ToPath GetRelationalDatabaseSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseSnapshotResponse' smart constructor.
data GetRelationalDatabaseSnapshotResponse = GetRelationalDatabaseSnapshotResponse'
  { relationalDatabaseSnapshot ::
      Lude.Maybe
        RelationalDatabaseSnapshot,
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

-- | Creates a value of 'GetRelationalDatabaseSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseSnapshot' - An object describing the specified database snapshot.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseSnapshotResponse
mkGetRelationalDatabaseSnapshotResponse pResponseStatus_ =
  GetRelationalDatabaseSnapshotResponse'
    { relationalDatabaseSnapshot =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object describing the specified database snapshot.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsRelationalDatabaseSnapshot :: Lens.Lens' GetRelationalDatabaseSnapshotResponse (Lude.Maybe RelationalDatabaseSnapshot)
getrsRelationalDatabaseSnapshot = Lens.lens (relationalDatabaseSnapshot :: GetRelationalDatabaseSnapshotResponse -> Lude.Maybe RelationalDatabaseSnapshot) (\s a -> s {relationalDatabaseSnapshot = a} :: GetRelationalDatabaseSnapshotResponse)
{-# DEPRECATED getrsRelationalDatabaseSnapshot "Use generic-lens or generic-optics with 'relationalDatabaseSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsResponseStatus :: Lens.Lens' GetRelationalDatabaseSnapshotResponse Lude.Int
getrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseSnapshotResponse)
{-# DEPRECATED getrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
