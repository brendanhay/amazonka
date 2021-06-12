{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstancesFromSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more new instances from a manual or automatic snapshot of
-- an instance.
--
-- The @create instances from snapshot@ operation supports tag-based access
-- control via request tags and resource tags applied to the resource
-- identified by @instance snapshot name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateInstancesFromSnapshot
  ( -- * Creating a Request
    CreateInstancesFromSnapshot (..),
    newCreateInstancesFromSnapshot,

    -- * Request Lenses
    createInstancesFromSnapshot_ipAddressType,
    createInstancesFromSnapshot_restoreDate,
    createInstancesFromSnapshot_userData,
    createInstancesFromSnapshot_addOns,
    createInstancesFromSnapshot_attachedDiskMapping,
    createInstancesFromSnapshot_instanceSnapshotName,
    createInstancesFromSnapshot_keyPairName,
    createInstancesFromSnapshot_useLatestRestorableAutoSnapshot,
    createInstancesFromSnapshot_tags,
    createInstancesFromSnapshot_sourceInstanceName,
    createInstancesFromSnapshot_instanceNames,
    createInstancesFromSnapshot_availabilityZone,
    createInstancesFromSnapshot_bundleId,

    -- * Destructuring the Response
    CreateInstancesFromSnapshotResponse (..),
    newCreateInstancesFromSnapshotResponse,

    -- * Response Lenses
    createInstancesFromSnapshotResponse_operations,
    createInstancesFromSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInstancesFromSnapshot' smart constructor.
data CreateInstancesFromSnapshot = CreateInstancesFromSnapshot'
  { -- | The IP address type for the instance.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    --
    -- The default value is @dualstack@.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | The date of the automatic snapshot to use for the new instance. Use the
    -- @get auto snapshots@ operation to identify the dates of the available
    -- automatic snapshots.
    --
    -- Constraints:
    --
    -- -   Must be specified in @YYYY-MM-DD@ format.
    --
    -- -   This parameter cannot be defined together with the
    --     @use latest restorable auto snapshot@ parameter. The @restore date@
    --     and @use latest restorable auto snapshot@ parameters are mutually
    --     exclusive.
    --
    -- -   Define this parameter only when creating a new instance from an
    --     automatic snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
    restoreDate :: Core.Maybe Core.Text,
    -- | You can create a launch script that configures a server with additional
    -- user data. For example, @apt-get -y update@.
    --
    -- Depending on the machine image you choose, the command to get software
    -- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
    -- Ubuntu use @apt-get@, and FreeBSD uses @pkg@. For a complete list, see
    -- the
    -- <https://lightsail.aws.amazon.com/ls/docs/getting-started/article/compare-options-choose-lightsail-instance-image Dev Guide>.
    userData :: Core.Maybe Core.Text,
    -- | An array of objects representing the add-ons to enable for the new
    -- instance.
    addOns :: Core.Maybe [AddOnRequest],
    -- | An object containing information about one or more disk mappings.
    attachedDiskMapping :: Core.Maybe (Core.HashMap Core.Text [DiskMap]),
    -- | The name of the instance snapshot on which you are basing your new
    -- instances. Use the get instance snapshots operation to return
    -- information about your existing snapshots.
    --
    -- Constraint:
    --
    -- -   This parameter cannot be defined together with the
    --     @source instance name@ parameter. The @instance snapshot name@ and
    --     @source instance name@ parameters are mutually exclusive.
    instanceSnapshotName :: Core.Maybe Core.Text,
    -- | The name for your key pair.
    keyPairName :: Core.Maybe Core.Text,
    -- | A Boolean value to indicate whether to use the latest available
    -- automatic snapshot.
    --
    -- Constraints:
    --
    -- -   This parameter cannot be defined together with the @restore date@
    --     parameter. The @use latest restorable auto snapshot@ and
    --     @restore date@ parameters are mutually exclusive.
    --
    -- -   Define this parameter only when creating a new instance from an
    --     automatic snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
    useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The name of the source instance from which the source automatic snapshot
    -- was created.
    --
    -- Constraints:
    --
    -- -   This parameter cannot be defined together with the
    --     @instance snapshot name@ parameter. The @source instance name@ and
    --     @instance snapshot name@ parameters are mutually exclusive.
    --
    -- -   Define this parameter only when creating a new instance from an
    --     automatic snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
    sourceInstanceName :: Core.Maybe Core.Text,
    -- | The names for your new instances.
    instanceNames :: [Core.Text],
    -- | The Availability Zone where you want to create your instances. Use the
    -- following formatting: @us-east-2a@ (case sensitive). You can get a list
    -- of Availability Zones by using the
    -- <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions>
    -- operation. Be sure to add the @include Availability Zones@ parameter to
    -- your request.
    availabilityZone :: Core.Text,
    -- | The bundle of specification information for your virtual private server
    -- (or /instance/), including the pricing plan (e.g., @micro_1_0@).
    bundleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInstancesFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'createInstancesFromSnapshot_ipAddressType' - The IP address type for the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
--
-- 'restoreDate', 'createInstancesFromSnapshot_restoreDate' - The date of the automatic snapshot to use for the new instance. Use the
-- @get auto snapshots@ operation to identify the dates of the available
-- automatic snapshots.
--
-- Constraints:
--
-- -   Must be specified in @YYYY-MM-DD@ format.
--
-- -   This parameter cannot be defined together with the
--     @use latest restorable auto snapshot@ parameter. The @restore date@
--     and @use latest restorable auto snapshot@ parameters are mutually
--     exclusive.
--
-- -   Define this parameter only when creating a new instance from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
--
-- 'userData', 'createInstancesFromSnapshot_userData' - You can create a launch script that configures a server with additional
-- user data. For example, @apt-get -y update@.
--
-- Depending on the machine image you choose, the command to get software
-- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
-- Ubuntu use @apt-get@, and FreeBSD uses @pkg@. For a complete list, see
-- the
-- <https://lightsail.aws.amazon.com/ls/docs/getting-started/article/compare-options-choose-lightsail-instance-image Dev Guide>.
--
-- 'addOns', 'createInstancesFromSnapshot_addOns' - An array of objects representing the add-ons to enable for the new
-- instance.
--
-- 'attachedDiskMapping', 'createInstancesFromSnapshot_attachedDiskMapping' - An object containing information about one or more disk mappings.
--
-- 'instanceSnapshotName', 'createInstancesFromSnapshot_instanceSnapshotName' - The name of the instance snapshot on which you are basing your new
-- instances. Use the get instance snapshots operation to return
-- information about your existing snapshots.
--
-- Constraint:
--
-- -   This parameter cannot be defined together with the
--     @source instance name@ parameter. The @instance snapshot name@ and
--     @source instance name@ parameters are mutually exclusive.
--
-- 'keyPairName', 'createInstancesFromSnapshot_keyPairName' - The name for your key pair.
--
-- 'useLatestRestorableAutoSnapshot', 'createInstancesFromSnapshot_useLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available
-- automatic snapshot.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the @restore date@
--     parameter. The @use latest restorable auto snapshot@ and
--     @restore date@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new instance from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
--
-- 'tags', 'createInstancesFromSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'sourceInstanceName', 'createInstancesFromSnapshot_sourceInstanceName' - The name of the source instance from which the source automatic snapshot
-- was created.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the
--     @instance snapshot name@ parameter. The @source instance name@ and
--     @instance snapshot name@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new instance from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
--
-- 'instanceNames', 'createInstancesFromSnapshot_instanceNames' - The names for your new instances.
--
-- 'availabilityZone', 'createInstancesFromSnapshot_availabilityZone' - The Availability Zone where you want to create your instances. Use the
-- following formatting: @us-east-2a@ (case sensitive). You can get a list
-- of Availability Zones by using the
-- <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions>
-- operation. Be sure to add the @include Availability Zones@ parameter to
-- your request.
--
-- 'bundleId', 'createInstancesFromSnapshot_bundleId' - The bundle of specification information for your virtual private server
-- (or /instance/), including the pricing plan (e.g., @micro_1_0@).
newCreateInstancesFromSnapshot ::
  -- | 'availabilityZone'
  Core.Text ->
  -- | 'bundleId'
  Core.Text ->
  CreateInstancesFromSnapshot
newCreateInstancesFromSnapshot
  pAvailabilityZone_
  pBundleId_ =
    CreateInstancesFromSnapshot'
      { ipAddressType =
          Core.Nothing,
        restoreDate = Core.Nothing,
        userData = Core.Nothing,
        addOns = Core.Nothing,
        attachedDiskMapping = Core.Nothing,
        instanceSnapshotName = Core.Nothing,
        keyPairName = Core.Nothing,
        useLatestRestorableAutoSnapshot = Core.Nothing,
        tags = Core.Nothing,
        sourceInstanceName = Core.Nothing,
        instanceNames = Core.mempty,
        availabilityZone = pAvailabilityZone_,
        bundleId = pBundleId_
      }

-- | The IP address type for the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
createInstancesFromSnapshot_ipAddressType :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe IpAddressType)
createInstancesFromSnapshot_ipAddressType = Lens.lens (\CreateInstancesFromSnapshot' {ipAddressType} -> ipAddressType) (\s@CreateInstancesFromSnapshot' {} a -> s {ipAddressType = a} :: CreateInstancesFromSnapshot)

-- | The date of the automatic snapshot to use for the new instance. Use the
-- @get auto snapshots@ operation to identify the dates of the available
-- automatic snapshots.
--
-- Constraints:
--
-- -   Must be specified in @YYYY-MM-DD@ format.
--
-- -   This parameter cannot be defined together with the
--     @use latest restorable auto snapshot@ parameter. The @restore date@
--     and @use latest restorable auto snapshot@ parameters are mutually
--     exclusive.
--
-- -   Define this parameter only when creating a new instance from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
createInstancesFromSnapshot_restoreDate :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
createInstancesFromSnapshot_restoreDate = Lens.lens (\CreateInstancesFromSnapshot' {restoreDate} -> restoreDate) (\s@CreateInstancesFromSnapshot' {} a -> s {restoreDate = a} :: CreateInstancesFromSnapshot)

-- | You can create a launch script that configures a server with additional
-- user data. For example, @apt-get -y update@.
--
-- Depending on the machine image you choose, the command to get software
-- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
-- Ubuntu use @apt-get@, and FreeBSD uses @pkg@. For a complete list, see
-- the
-- <https://lightsail.aws.amazon.com/ls/docs/getting-started/article/compare-options-choose-lightsail-instance-image Dev Guide>.
createInstancesFromSnapshot_userData :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
createInstancesFromSnapshot_userData = Lens.lens (\CreateInstancesFromSnapshot' {userData} -> userData) (\s@CreateInstancesFromSnapshot' {} a -> s {userData = a} :: CreateInstancesFromSnapshot)

-- | An array of objects representing the add-ons to enable for the new
-- instance.
createInstancesFromSnapshot_addOns :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe [AddOnRequest])
createInstancesFromSnapshot_addOns = Lens.lens (\CreateInstancesFromSnapshot' {addOns} -> addOns) (\s@CreateInstancesFromSnapshot' {} a -> s {addOns = a} :: CreateInstancesFromSnapshot) Core.. Lens.mapping Lens._Coerce

-- | An object containing information about one or more disk mappings.
createInstancesFromSnapshot_attachedDiskMapping :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe (Core.HashMap Core.Text [DiskMap]))
createInstancesFromSnapshot_attachedDiskMapping = Lens.lens (\CreateInstancesFromSnapshot' {attachedDiskMapping} -> attachedDiskMapping) (\s@CreateInstancesFromSnapshot' {} a -> s {attachedDiskMapping = a} :: CreateInstancesFromSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The name of the instance snapshot on which you are basing your new
-- instances. Use the get instance snapshots operation to return
-- information about your existing snapshots.
--
-- Constraint:
--
-- -   This parameter cannot be defined together with the
--     @source instance name@ parameter. The @instance snapshot name@ and
--     @source instance name@ parameters are mutually exclusive.
createInstancesFromSnapshot_instanceSnapshotName :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
createInstancesFromSnapshot_instanceSnapshotName = Lens.lens (\CreateInstancesFromSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@CreateInstancesFromSnapshot' {} a -> s {instanceSnapshotName = a} :: CreateInstancesFromSnapshot)

-- | The name for your key pair.
createInstancesFromSnapshot_keyPairName :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
createInstancesFromSnapshot_keyPairName = Lens.lens (\CreateInstancesFromSnapshot' {keyPairName} -> keyPairName) (\s@CreateInstancesFromSnapshot' {} a -> s {keyPairName = a} :: CreateInstancesFromSnapshot)

-- | A Boolean value to indicate whether to use the latest available
-- automatic snapshot.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the @restore date@
--     parameter. The @use latest restorable auto snapshot@ and
--     @restore date@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new instance from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
createInstancesFromSnapshot_useLatestRestorableAutoSnapshot :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Bool)
createInstancesFromSnapshot_useLatestRestorableAutoSnapshot = Lens.lens (\CreateInstancesFromSnapshot' {useLatestRestorableAutoSnapshot} -> useLatestRestorableAutoSnapshot) (\s@CreateInstancesFromSnapshot' {} a -> s {useLatestRestorableAutoSnapshot = a} :: CreateInstancesFromSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createInstancesFromSnapshot_tags :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe [Tag])
createInstancesFromSnapshot_tags = Lens.lens (\CreateInstancesFromSnapshot' {tags} -> tags) (\s@CreateInstancesFromSnapshot' {} a -> s {tags = a} :: CreateInstancesFromSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The name of the source instance from which the source automatic snapshot
-- was created.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the
--     @instance snapshot name@ parameter. The @source instance name@ and
--     @instance snapshot name@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new instance from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
createInstancesFromSnapshot_sourceInstanceName :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
createInstancesFromSnapshot_sourceInstanceName = Lens.lens (\CreateInstancesFromSnapshot' {sourceInstanceName} -> sourceInstanceName) (\s@CreateInstancesFromSnapshot' {} a -> s {sourceInstanceName = a} :: CreateInstancesFromSnapshot)

-- | The names for your new instances.
createInstancesFromSnapshot_instanceNames :: Lens.Lens' CreateInstancesFromSnapshot [Core.Text]
createInstancesFromSnapshot_instanceNames = Lens.lens (\CreateInstancesFromSnapshot' {instanceNames} -> instanceNames) (\s@CreateInstancesFromSnapshot' {} a -> s {instanceNames = a} :: CreateInstancesFromSnapshot) Core.. Lens._Coerce

-- | The Availability Zone where you want to create your instances. Use the
-- following formatting: @us-east-2a@ (case sensitive). You can get a list
-- of Availability Zones by using the
-- <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions>
-- operation. Be sure to add the @include Availability Zones@ parameter to
-- your request.
createInstancesFromSnapshot_availabilityZone :: Lens.Lens' CreateInstancesFromSnapshot Core.Text
createInstancesFromSnapshot_availabilityZone = Lens.lens (\CreateInstancesFromSnapshot' {availabilityZone} -> availabilityZone) (\s@CreateInstancesFromSnapshot' {} a -> s {availabilityZone = a} :: CreateInstancesFromSnapshot)

-- | The bundle of specification information for your virtual private server
-- (or /instance/), including the pricing plan (e.g., @micro_1_0@).
createInstancesFromSnapshot_bundleId :: Lens.Lens' CreateInstancesFromSnapshot Core.Text
createInstancesFromSnapshot_bundleId = Lens.lens (\CreateInstancesFromSnapshot' {bundleId} -> bundleId) (\s@CreateInstancesFromSnapshot' {} a -> s {bundleId = a} :: CreateInstancesFromSnapshot)

instance Core.AWSRequest CreateInstancesFromSnapshot where
  type
    AWSResponse CreateInstancesFromSnapshot =
      CreateInstancesFromSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstancesFromSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateInstancesFromSnapshot

instance Core.NFData CreateInstancesFromSnapshot

instance Core.ToHeaders CreateInstancesFromSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateInstancesFromSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateInstancesFromSnapshot where
  toJSON CreateInstancesFromSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ipAddressType" Core..=) Core.<$> ipAddressType,
            ("restoreDate" Core..=) Core.<$> restoreDate,
            ("userData" Core..=) Core.<$> userData,
            ("addOns" Core..=) Core.<$> addOns,
            ("attachedDiskMapping" Core..=)
              Core.<$> attachedDiskMapping,
            ("instanceSnapshotName" Core..=)
              Core.<$> instanceSnapshotName,
            ("keyPairName" Core..=) Core.<$> keyPairName,
            ("useLatestRestorableAutoSnapshot" Core..=)
              Core.<$> useLatestRestorableAutoSnapshot,
            ("tags" Core..=) Core.<$> tags,
            ("sourceInstanceName" Core..=)
              Core.<$> sourceInstanceName,
            Core.Just ("instanceNames" Core..= instanceNames),
            Core.Just
              ("availabilityZone" Core..= availabilityZone),
            Core.Just ("bundleId" Core..= bundleId)
          ]
      )

instance Core.ToPath CreateInstancesFromSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CreateInstancesFromSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateInstancesFromSnapshotResponse' smart constructor.
data CreateInstancesFromSnapshotResponse = CreateInstancesFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInstancesFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createInstancesFromSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createInstancesFromSnapshotResponse_httpStatus' - The response's http status code.
newCreateInstancesFromSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateInstancesFromSnapshotResponse
newCreateInstancesFromSnapshotResponse pHttpStatus_ =
  CreateInstancesFromSnapshotResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createInstancesFromSnapshotResponse_operations :: Lens.Lens' CreateInstancesFromSnapshotResponse (Core.Maybe [Operation])
createInstancesFromSnapshotResponse_operations = Lens.lens (\CreateInstancesFromSnapshotResponse' {operations} -> operations) (\s@CreateInstancesFromSnapshotResponse' {} a -> s {operations = a} :: CreateInstancesFromSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createInstancesFromSnapshotResponse_httpStatus :: Lens.Lens' CreateInstancesFromSnapshotResponse Core.Int
createInstancesFromSnapshotResponse_httpStatus = Lens.lens (\CreateInstancesFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateInstancesFromSnapshotResponse' {} a -> s {httpStatus = a} :: CreateInstancesFromSnapshotResponse)

instance
  Core.NFData
    CreateInstancesFromSnapshotResponse
