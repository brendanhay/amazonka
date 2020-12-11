{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database from an existing database snapshot in Amazon Lightsail.
--
-- You can create a new database from a snapshot in if something goes wrong with your original database, or to change it to a different plan, such as a high availability or standard plan.
-- The @create relational database from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by relationalDatabaseSnapshotName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
  ( -- * Creating a request
    CreateRelationalDatabaseFromSnapshot (..),
    mkCreateRelationalDatabaseFromSnapshot,

    -- ** Request lenses
    crdfsSourceRelationalDatabaseName,
    crdfsRelationalDatabaseBundleId,
    crdfsPubliclyAccessible,
    crdfsUseLatestRestorableTime,
    crdfsRestoreTime,
    crdfsAvailabilityZone,
    crdfsRelationalDatabaseSnapshotName,
    crdfsTags,
    crdfsRelationalDatabaseName,

    -- * Destructuring the response
    CreateRelationalDatabaseFromSnapshotResponse (..),
    mkCreateRelationalDatabaseFromSnapshotResponse,

    -- ** Response lenses
    crdfsrsOperations,
    crdfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRelationalDatabaseFromSnapshot' smart constructor.
data CreateRelationalDatabaseFromSnapshot = CreateRelationalDatabaseFromSnapshot'
  { sourceRelationalDatabaseName ::
      Lude.Maybe
        Lude.Text,
    relationalDatabaseBundleId ::
      Lude.Maybe
        Lude.Text,
    publiclyAccessible ::
      Lude.Maybe
        Lude.Bool,
    useLatestRestorableTime ::
      Lude.Maybe
        Lude.Bool,
    restoreTime ::
      Lude.Maybe
        Lude.Timestamp,
    availabilityZone ::
      Lude.Maybe
        Lude.Text,
    relationalDatabaseSnapshotName ::
      Lude.Maybe
        Lude.Text,
    tags ::
      Lude.Maybe [Tag],
    relationalDatabaseName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRelationalDatabaseFromSnapshot' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
-- * 'publiclyAccessible' - Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
-- * 'relationalDatabaseBundleId' - The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
-- When creating a new database from a snapshot, you cannot choose a bundle that is smaller than the bundle of the source database.
-- * 'relationalDatabaseName' - The name to use for your new database.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
-- * 'relationalDatabaseSnapshotName' - The name of the database snapshot from which to create your new database.
-- * 'restoreTime' - The date and time to restore your database from.
--
-- Constraints:
--
--     * Must be before the latest restorable time for the database.
--
--
--     * Cannot be specified if the @use latest restorable time@ parameter is @true@ .
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use a restore time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the restore time.
--
--
-- * 'sourceRelationalDatabaseName' - The name of the source database.
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
-- * 'useLatestRestorableTime' - Specifies whether your database is restored from the latest backup time. A value of @true@ restores from the latest backup time.
--
-- Default: @false@
-- Constraints: Cannot be specified if the @restore time@ parameter is provided.
mkCreateRelationalDatabaseFromSnapshot ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  CreateRelationalDatabaseFromSnapshot
mkCreateRelationalDatabaseFromSnapshot pRelationalDatabaseName_ =
  CreateRelationalDatabaseFromSnapshot'
    { sourceRelationalDatabaseName =
        Lude.Nothing,
      relationalDatabaseBundleId = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      useLatestRestorableTime = Lude.Nothing,
      restoreTime = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      relationalDatabaseSnapshotName = Lude.Nothing,
      tags = Lude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The name of the source database.
--
-- /Note:/ Consider using 'sourceRelationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsSourceRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Text)
crdfsSourceRelationalDatabaseName = Lens.lens (sourceRelationalDatabaseName :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceRelationalDatabaseName = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsSourceRelationalDatabaseName "Use generic-lens or generic-optics with 'sourceRelationalDatabaseName' instead." #-}

-- | The bundle ID for your new database. A bundle describes the performance specifications for your database.
--
-- You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
-- When creating a new database from a snapshot, you cannot choose a bundle that is smaller than the bundle of the source database.
--
-- /Note:/ Consider using 'relationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsRelationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Text)
crdfsRelationalDatabaseBundleId = Lens.lens (relationalDatabaseBundleId :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {relationalDatabaseBundleId = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsRelationalDatabaseBundleId "Use generic-lens or generic-optics with 'relationalDatabaseBundleId' instead." #-}

-- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsPubliclyAccessible :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Bool)
crdfsPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | Specifies whether your database is restored from the latest backup time. A value of @true@ restores from the latest backup time.
--
-- Default: @false@
-- Constraints: Cannot be specified if the @restore time@ parameter is provided.
--
-- /Note:/ Consider using 'useLatestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsUseLatestRestorableTime :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Bool)
crdfsUseLatestRestorableTime = Lens.lens (useLatestRestorableTime :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableTime = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsUseLatestRestorableTime "Use generic-lens or generic-optics with 'useLatestRestorableTime' instead." #-}

-- | The date and time to restore your database from.
--
-- Constraints:
--
--     * Must be before the latest restorable time for the database.
--
--
--     * Cannot be specified if the @use latest restorable time@ parameter is @true@ .
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * Specified in the Unix time format.
-- For example, if you wish to use a restore time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the restore time.
--
--
--
-- /Note:/ Consider using 'restoreTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsRestoreTime :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Timestamp)
crdfsRestoreTime = Lens.lens (restoreTime :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {restoreTime = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsRestoreTime "Use generic-lens or generic-optics with 'restoreTime' instead." #-}

-- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsAvailabilityZone :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Text)
crdfsAvailabilityZone = Lens.lens (availabilityZone :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the database snapshot from which to create your new database.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsRelationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe Lude.Text)
crdfsRelationalDatabaseSnapshotName = Lens.lens (relationalDatabaseSnapshotName :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {relationalDatabaseSnapshotName = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsTags :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Lude.Maybe [Tag])
crdfsTags = Lens.lens (tags :: CreateRelationalDatabaseFromSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name to use for your new database.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot Lude.Text
crdfsRelationalDatabaseName = Lens.lens (relationalDatabaseName :: CreateRelationalDatabaseFromSnapshot -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: CreateRelationalDatabaseFromSnapshot)
{-# DEPRECATED crdfsRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest CreateRelationalDatabaseFromSnapshot where
  type
    Rs CreateRelationalDatabaseFromSnapshot =
      CreateRelationalDatabaseFromSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseFromSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRelationalDatabaseFromSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateRelationalDatabaseFromSnapshot" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRelationalDatabaseFromSnapshot where
  toJSON CreateRelationalDatabaseFromSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sourceRelationalDatabaseName" Lude..=)
              Lude.<$> sourceRelationalDatabaseName,
            ("relationalDatabaseBundleId" Lude..=)
              Lude.<$> relationalDatabaseBundleId,
            ("publiclyAccessible" Lude..=) Lude.<$> publiclyAccessible,
            ("useLatestRestorableTime" Lude..=)
              Lude.<$> useLatestRestorableTime,
            ("restoreTime" Lude..=) Lude.<$> restoreTime,
            ("availabilityZone" Lude..=) Lude.<$> availabilityZone,
            ("relationalDatabaseSnapshotName" Lude..=)
              Lude.<$> relationalDatabaseSnapshotName,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath CreateRelationalDatabaseFromSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRelationalDatabaseFromSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRelationalDatabaseFromSnapshotResponse' smart constructor.
data CreateRelationalDatabaseFromSnapshotResponse = CreateRelationalDatabaseFromSnapshotResponse'
  { operations ::
      Lude.Maybe
        [Operation],
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

-- | Creates a value of 'CreateRelationalDatabaseFromSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateRelationalDatabaseFromSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRelationalDatabaseFromSnapshotResponse
mkCreateRelationalDatabaseFromSnapshotResponse pResponseStatus_ =
  CreateRelationalDatabaseFromSnapshotResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsrsOperations :: Lens.Lens' CreateRelationalDatabaseFromSnapshotResponse (Lude.Maybe [Operation])
crdfsrsOperations = Lens.lens (operations :: CreateRelationalDatabaseFromSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateRelationalDatabaseFromSnapshotResponse)
{-# DEPRECATED crdfsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdfsrsResponseStatus :: Lens.Lens' CreateRelationalDatabaseFromSnapshotResponse Lude.Int
crdfsrsResponseStatus = Lens.lens (responseStatus :: CreateRelationalDatabaseFromSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRelationalDatabaseFromSnapshotResponse)
{-# DEPRECATED crdfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
