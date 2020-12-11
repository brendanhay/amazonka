{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StartRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific database from a stopped state in Amazon Lightsail. To restart a database, use the @reboot relational database@ operation.
--
-- The @start relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StartRelationalDatabase
  ( -- * Creating a request
    StartRelationalDatabase (..),
    mkStartRelationalDatabase,

    -- ** Request lenses
    sRelationalDatabaseName,

    -- * Destructuring the response
    StartRelationalDatabaseResponse (..),
    mkStartRelationalDatabaseResponse,

    -- ** Response lenses
    srdrsOperations,
    srdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartRelationalDatabase' smart constructor.
newtype StartRelationalDatabase = StartRelationalDatabase'
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

-- | Creates a value of 'StartRelationalDatabase' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseName' - The name of your database to start.
mkStartRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  StartRelationalDatabase
mkStartRelationalDatabase pRelationalDatabaseName_ =
  StartRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of your database to start.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRelationalDatabaseName :: Lens.Lens' StartRelationalDatabase Lude.Text
sRelationalDatabaseName = Lens.lens (relationalDatabaseName :: StartRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: StartRelationalDatabase)
{-# DEPRECATED sRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest StartRelationalDatabase where
  type Rs StartRelationalDatabase = StartRelationalDatabaseResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartRelationalDatabaseResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartRelationalDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.StartRelationalDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartRelationalDatabase where
  toJSON StartRelationalDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath StartRelationalDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery StartRelationalDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartRelationalDatabaseResponse' smart constructor.
data StartRelationalDatabaseResponse = StartRelationalDatabaseResponse'
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

-- | Creates a value of 'StartRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkStartRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartRelationalDatabaseResponse
mkStartRelationalDatabaseResponse pResponseStatus_ =
  StartRelationalDatabaseResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdrsOperations :: Lens.Lens' StartRelationalDatabaseResponse (Lude.Maybe [Operation])
srdrsOperations = Lens.lens (operations :: StartRelationalDatabaseResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: StartRelationalDatabaseResponse)
{-# DEPRECATED srdrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdrsResponseStatus :: Lens.Lens' StartRelationalDatabaseResponse Lude.Int
srdrsResponseStatus = Lens.lens (responseStatus :: StartRelationalDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartRelationalDatabaseResponse)
{-# DEPRECATED srdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
