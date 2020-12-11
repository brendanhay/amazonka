{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstancesFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more new instances from a manual or automatic snapshot of an instance.
--
-- The @create instances from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @instance snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateInstancesFromSnapshot
  ( -- * Creating a request
    CreateInstancesFromSnapshot (..),
    mkCreateInstancesFromSnapshot,

    -- ** Request lenses
    cifsUseLatestRestorableAutoSnapshot,
    cifsInstanceSnapshotName,
    cifsAddOns,
    cifsUserData,
    cifsRestoreDate,
    cifsKeyPairName,
    cifsSourceInstanceName,
    cifsAttachedDiskMapping,
    cifsTags,
    cifsInstanceNames,
    cifsAvailabilityZone,
    cifsBundleId,

    -- * Destructuring the response
    CreateInstancesFromSnapshotResponse (..),
    mkCreateInstancesFromSnapshotResponse,

    -- ** Response lenses
    cifsrsOperations,
    cifsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstancesFromSnapshot' smart constructor.
data CreateInstancesFromSnapshot = CreateInstancesFromSnapshot'
  { useLatestRestorableAutoSnapshot ::
      Lude.Maybe Lude.Bool,
    instanceSnapshotName ::
      Lude.Maybe Lude.Text,
    addOns :: Lude.Maybe [AddOnRequest],
    userData :: Lude.Maybe Lude.Text,
    restoreDate :: Lude.Maybe Lude.Text,
    keyPairName :: Lude.Maybe Lude.Text,
    sourceInstanceName ::
      Lude.Maybe Lude.Text,
    attachedDiskMapping ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([DiskMap])
        ),
    tags :: Lude.Maybe [Tag],
    instanceNames :: [Lude.Text],
    availabilityZone :: Lude.Text,
    bundleId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstancesFromSnapshot' with the minimum fields required to make a request.
--
-- * 'addOns' - An array of objects representing the add-ons to enable for the new instance.
-- * 'attachedDiskMapping' - An object containing information about one or more disk mappings.
-- * 'availabilityZone' - The Availability Zone where you want to create your instances. Use the following formatting: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
-- * 'bundleId' - The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
-- * 'instanceNames' - The names for your new instances.
-- * 'instanceSnapshotName' - The name of the instance snapshot on which you are basing your new instances. Use the get instance snapshots operation to return information about your existing snapshots.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source instance name@ parameter. The @instance snapshot name@ and @source instance name@ parameters are mutually exclusive.
--
--
-- * 'keyPairName' - The name for your key pair.
-- * 'restoreDate' - The date of the automatic snapshot to use for the new instance. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'sourceInstanceName' - The name of the source instance from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @instance snapshot name@ parameter. The @source instance name@ and @instance snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
-- * 'useLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'userData' - You can create a launch script that configures a server with additional user data. For example, @apt-get -y update@ .
mkCreateInstancesFromSnapshot ::
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'bundleId'
  Lude.Text ->
  CreateInstancesFromSnapshot
mkCreateInstancesFromSnapshot pAvailabilityZone_ pBundleId_ =
  CreateInstancesFromSnapshot'
    { useLatestRestorableAutoSnapshot =
        Lude.Nothing,
      instanceSnapshotName = Lude.Nothing,
      addOns = Lude.Nothing,
      userData = Lude.Nothing,
      restoreDate = Lude.Nothing,
      keyPairName = Lude.Nothing,
      sourceInstanceName = Lude.Nothing,
      attachedDiskMapping = Lude.Nothing,
      tags = Lude.Nothing,
      instanceNames = Lude.mempty,
      availabilityZone = pAvailabilityZone_,
      bundleId = pBundleId_
    }

-- | A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'useLatestRestorableAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsUseLatestRestorableAutoSnapshot :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe Lude.Bool)
cifsUseLatestRestorableAutoSnapshot = Lens.lens (useLatestRestorableAutoSnapshot :: CreateInstancesFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableAutoSnapshot = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsUseLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead." #-}

-- | The name of the instance snapshot on which you are basing your new instances. Use the get instance snapshots operation to return information about your existing snapshots.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source instance name@ parameter. The @instance snapshot name@ and @source instance name@ parameters are mutually exclusive.
--
--
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsInstanceSnapshotName :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe Lude.Text)
cifsInstanceSnapshotName = Lens.lens (instanceSnapshotName :: CreateInstancesFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {instanceSnapshotName = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsInstanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead." #-}

-- | An array of objects representing the add-ons to enable for the new instance.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsAddOns :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe [AddOnRequest])
cifsAddOns = Lens.lens (addOns :: CreateInstancesFromSnapshot -> Lude.Maybe [AddOnRequest]) (\s a -> s {addOns = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | You can create a launch script that configures a server with additional user data. For example, @apt-get -y update@ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsUserData :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe Lude.Text)
cifsUserData = Lens.lens (userData :: CreateInstancesFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The date of the automatic snapshot to use for the new instance. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'restoreDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsRestoreDate :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe Lude.Text)
cifsRestoreDate = Lens.lens (restoreDate :: CreateInstancesFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {restoreDate = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsRestoreDate "Use generic-lens or generic-optics with 'restoreDate' instead." #-}

-- | The name for your key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsKeyPairName :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe Lude.Text)
cifsKeyPairName = Lens.lens (keyPairName :: CreateInstancesFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {keyPairName = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

-- | The name of the source instance from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @instance snapshot name@ parameter. The @source instance name@ and @instance snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'sourceInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsSourceInstanceName :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe Lude.Text)
cifsSourceInstanceName = Lens.lens (sourceInstanceName :: CreateInstancesFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceInstanceName = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsSourceInstanceName "Use generic-lens or generic-optics with 'sourceInstanceName' instead." #-}

-- | An object containing information about one or more disk mappings.
--
-- /Note:/ Consider using 'attachedDiskMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsAttachedDiskMapping :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe (Lude.HashMap Lude.Text ([DiskMap])))
cifsAttachedDiskMapping = Lens.lens (attachedDiskMapping :: CreateInstancesFromSnapshot -> Lude.Maybe (Lude.HashMap Lude.Text ([DiskMap]))) (\s a -> s {attachedDiskMapping = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsAttachedDiskMapping "Use generic-lens or generic-optics with 'attachedDiskMapping' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsTags :: Lens.Lens' CreateInstancesFromSnapshot (Lude.Maybe [Tag])
cifsTags = Lens.lens (tags :: CreateInstancesFromSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The names for your new instances.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsInstanceNames :: Lens.Lens' CreateInstancesFromSnapshot [Lude.Text]
cifsInstanceNames = Lens.lens (instanceNames :: CreateInstancesFromSnapshot -> [Lude.Text]) (\s a -> s {instanceNames = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

-- | The Availability Zone where you want to create your instances. Use the following formatting: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsAvailabilityZone :: Lens.Lens' CreateInstancesFromSnapshot Lude.Text
cifsAvailabilityZone = Lens.lens (availabilityZone :: CreateInstancesFromSnapshot -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsBundleId :: Lens.Lens' CreateInstancesFromSnapshot Lude.Text
cifsBundleId = Lens.lens (bundleId :: CreateInstancesFromSnapshot -> Lude.Text) (\s a -> s {bundleId = a} :: CreateInstancesFromSnapshot)
{-# DEPRECATED cifsBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

instance Lude.AWSRequest CreateInstancesFromSnapshot where
  type
    Rs CreateInstancesFromSnapshot =
      CreateInstancesFromSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInstancesFromSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstancesFromSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateInstancesFromSnapshot" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInstancesFromSnapshot where
  toJSON CreateInstancesFromSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("useLatestRestorableAutoSnapshot" Lude..=)
              Lude.<$> useLatestRestorableAutoSnapshot,
            ("instanceSnapshotName" Lude..=) Lude.<$> instanceSnapshotName,
            ("addOns" Lude..=) Lude.<$> addOns,
            ("userData" Lude..=) Lude.<$> userData,
            ("restoreDate" Lude..=) Lude.<$> restoreDate,
            ("keyPairName" Lude..=) Lude.<$> keyPairName,
            ("sourceInstanceName" Lude..=) Lude.<$> sourceInstanceName,
            ("attachedDiskMapping" Lude..=) Lude.<$> attachedDiskMapping,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("instanceNames" Lude..= instanceNames),
            Lude.Just ("availabilityZone" Lude..= availabilityZone),
            Lude.Just ("bundleId" Lude..= bundleId)
          ]
      )

instance Lude.ToPath CreateInstancesFromSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstancesFromSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateInstancesFromSnapshotResponse' smart constructor.
data CreateInstancesFromSnapshotResponse = CreateInstancesFromSnapshotResponse'
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

-- | Creates a value of 'CreateInstancesFromSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateInstancesFromSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstancesFromSnapshotResponse
mkCreateInstancesFromSnapshotResponse pResponseStatus_ =
  CreateInstancesFromSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsrsOperations :: Lens.Lens' CreateInstancesFromSnapshotResponse (Lude.Maybe [Operation])
cifsrsOperations = Lens.lens (operations :: CreateInstancesFromSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateInstancesFromSnapshotResponse)
{-# DEPRECATED cifsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsrsResponseStatus :: Lens.Lens' CreateInstancesFromSnapshotResponse Lude.Int
cifsrsResponseStatus = Lens.lens (responseStatus :: CreateInstancesFromSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstancesFromSnapshotResponse)
{-# DEPRECATED cifsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
