{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB instance. The source DB instance must be in the @available@ or @storage-optimization@ state.
module Network.AWS.RDS.CreateDBSnapshot
  ( -- * Creating a request
    CreateDBSnapshot (..),
    mkCreateDBSnapshot,

    -- ** Request lenses
    cdbsDBSnapshotIdentifier,
    cdbsDBInstanceIdentifier,
    cdbsTags,

    -- * Destructuring the response
    CreateDBSnapshotResponse (..),
    mkCreateDBSnapshotResponse,

    -- ** Response lenses
    cdsrsDBSnapshot,
    cdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBSnapshot' smart constructor.
data CreateDBSnapshot = CreateDBSnapshot'
  { -- | The identifier for the DB snapshot.
    --
    -- Constraints:
    --
    --     * Can't be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-snapshot-id@
    dbSnapshotIdentifier :: Lude.Text,
    -- | The identifier of the DB instance that you want to create the snapshot of.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBSnapshot' with the minimum fields required to make a request.
--
-- * 'dbSnapshotIdentifier' - The identifier for the DB snapshot.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
-- * 'dbInstanceIdentifier' - The identifier of the DB instance that you want to create the snapshot of.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
-- * 'tags' -
mkCreateDBSnapshot ::
  -- | 'dbSnapshotIdentifier'
  Lude.Text ->
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  CreateDBSnapshot
mkCreateDBSnapshot pDBSnapshotIdentifier_ pDBInstanceIdentifier_ =
  CreateDBSnapshot'
    { dbSnapshotIdentifier = pDBSnapshotIdentifier_,
      dbInstanceIdentifier = pDBInstanceIdentifier_,
      tags = Lude.Nothing
    }

-- | The identifier for the DB snapshot.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsDBSnapshotIdentifier :: Lens.Lens' CreateDBSnapshot Lude.Text
cdbsDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: CreateDBSnapshot -> Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: CreateDBSnapshot)
{-# DEPRECATED cdbsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

-- | The identifier of the DB instance that you want to create the snapshot of.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsDBInstanceIdentifier :: Lens.Lens' CreateDBSnapshot Lude.Text
cdbsDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: CreateDBSnapshot -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: CreateDBSnapshot)
{-# DEPRECATED cdbsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsTags :: Lens.Lens' CreateDBSnapshot (Lude.Maybe [Tag])
cdbsTags = Lens.lens (tags :: CreateDBSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBSnapshot)
{-# DEPRECATED cdbsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDBSnapshot where
  type Rs CreateDBSnapshot = CreateDBSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBSnapshotResult"
      ( \s h x ->
          CreateDBSnapshotResponse'
            Lude.<$> (x Lude..@? "DBSnapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBSnapshot where
  toQuery CreateDBSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateDBSnapshotResponse' smart constructor.
data CreateDBSnapshotResponse = CreateDBSnapshotResponse'
  { dbSnapshot :: Lude.Maybe DBSnapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshot' -
-- * 'responseStatus' - The response status code.
mkCreateDBSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBSnapshotResponse
mkCreateDBSnapshotResponse pResponseStatus_ =
  CreateDBSnapshotResponse'
    { dbSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsDBSnapshot :: Lens.Lens' CreateDBSnapshotResponse (Lude.Maybe DBSnapshot)
cdsrsDBSnapshot = Lens.lens (dbSnapshot :: CreateDBSnapshotResponse -> Lude.Maybe DBSnapshot) (\s a -> s {dbSnapshot = a} :: CreateDBSnapshotResponse)
{-# DEPRECATED cdsrsDBSnapshot "Use generic-lens or generic-optics with 'dbSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsResponseStatus :: Lens.Lens' CreateDBSnapshotResponse Lude.Int
cdsrsResponseStatus = Lens.lens (responseStatus :: CreateDBSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBSnapshotResponse)
{-# DEPRECATED cdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
