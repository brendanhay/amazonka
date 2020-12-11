{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.RebootRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific database in Amazon Lightsail.
--
-- The @reboot relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.RebootRelationalDatabase
  ( -- * Creating a request
    RebootRelationalDatabase (..),
    mkRebootRelationalDatabase,

    -- ** Request lenses
    rrdRelationalDatabaseName,

    -- * Destructuring the response
    RebootRelationalDatabaseResponse (..),
    mkRebootRelationalDatabaseResponse,

    -- ** Response lenses
    rrdrsOperations,
    rrdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootRelationalDatabase' smart constructor.
newtype RebootRelationalDatabase = RebootRelationalDatabase'
  { relationalDatabaseName ::
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

-- | Creates a value of 'RebootRelationalDatabase' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseName' - The name of your database to reboot.
mkRebootRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  RebootRelationalDatabase
mkRebootRelationalDatabase pRelationalDatabaseName_ =
  RebootRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of your database to reboot.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdRelationalDatabaseName :: Lens.Lens' RebootRelationalDatabase Lude.Text
rrdRelationalDatabaseName = Lens.lens (relationalDatabaseName :: RebootRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: RebootRelationalDatabase)
{-# DEPRECATED rrdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest RebootRelationalDatabase where
  type Rs RebootRelationalDatabase = RebootRelationalDatabaseResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          RebootRelationalDatabaseResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootRelationalDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.RebootRelationalDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootRelationalDatabase where
  toJSON RebootRelationalDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath RebootRelationalDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootRelationalDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootRelationalDatabaseResponse' smart constructor.
data RebootRelationalDatabaseResponse = RebootRelationalDatabaseResponse'
  { operations ::
      Lude.Maybe [Operation],
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

-- | Creates a value of 'RebootRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkRebootRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootRelationalDatabaseResponse
mkRebootRelationalDatabaseResponse pResponseStatus_ =
  RebootRelationalDatabaseResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdrsOperations :: Lens.Lens' RebootRelationalDatabaseResponse (Lude.Maybe [Operation])
rrdrsOperations = Lens.lens (operations :: RebootRelationalDatabaseResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: RebootRelationalDatabaseResponse)
{-# DEPRECATED rrdrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdrsResponseStatus :: Lens.Lens' RebootRelationalDatabaseResponse Lude.Int
rrdrsResponseStatus = Lens.lens (responseStatus :: RebootRelationalDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootRelationalDatabaseResponse)
{-# DEPRECATED rrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
