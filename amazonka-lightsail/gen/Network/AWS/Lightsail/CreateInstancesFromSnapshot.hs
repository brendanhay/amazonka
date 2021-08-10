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
import qualified Network.AWS.Prelude as Prelude
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
    ipAddressType :: Prelude.Maybe IpAddressType,
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
    restoreDate :: Prelude.Maybe Prelude.Text,
    -- | You can create a launch script that configures a server with additional
    -- user data. For example, @apt-get -y update@.
    --
    -- Depending on the machine image you choose, the command to get software
    -- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
    -- Ubuntu use @apt-get@, and FreeBSD uses @pkg@. For a complete list, see
    -- the
    -- <https://lightsail.aws.amazon.com/ls/docs/getting-started/article/compare-options-choose-lightsail-instance-image Dev Guide>.
    userData :: Prelude.Maybe Prelude.Text,
    -- | An array of objects representing the add-ons to enable for the new
    -- instance.
    addOns :: Prelude.Maybe [AddOnRequest],
    -- | An object containing information about one or more disk mappings.
    attachedDiskMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text [DiskMap]),
    -- | The name of the instance snapshot on which you are basing your new
    -- instances. Use the get instance snapshots operation to return
    -- information about your existing snapshots.
    --
    -- Constraint:
    --
    -- -   This parameter cannot be defined together with the
    --     @source instance name@ parameter. The @instance snapshot name@ and
    --     @source instance name@ parameters are mutually exclusive.
    instanceSnapshotName :: Prelude.Maybe Prelude.Text,
    -- | The name for your key pair.
    keyPairName :: Prelude.Maybe Prelude.Text,
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
    useLatestRestorableAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
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
    sourceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The names for your new instances.
    instanceNames :: [Prelude.Text],
    -- | The Availability Zone where you want to create your instances. Use the
    -- following formatting: @us-east-2a@ (case sensitive). You can get a list
    -- of Availability Zones by using the
    -- <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions>
    -- operation. Be sure to add the @include Availability Zones@ parameter to
    -- your request.
    availabilityZone :: Prelude.Text,
    -- | The bundle of specification information for your virtual private server
    -- (or /instance/), including the pricing plan (e.g., @micro_1_0@).
    bundleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'bundleId'
  Prelude.Text ->
  CreateInstancesFromSnapshot
newCreateInstancesFromSnapshot
  pAvailabilityZone_
  pBundleId_ =
    CreateInstancesFromSnapshot'
      { ipAddressType =
          Prelude.Nothing,
        restoreDate = Prelude.Nothing,
        userData = Prelude.Nothing,
        addOns = Prelude.Nothing,
        attachedDiskMapping = Prelude.Nothing,
        instanceSnapshotName = Prelude.Nothing,
        keyPairName = Prelude.Nothing,
        useLatestRestorableAutoSnapshot =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceInstanceName = Prelude.Nothing,
        instanceNames = Prelude.mempty,
        availabilityZone = pAvailabilityZone_,
        bundleId = pBundleId_
      }

-- | The IP address type for the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
createInstancesFromSnapshot_ipAddressType :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe IpAddressType)
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
createInstancesFromSnapshot_restoreDate :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe Prelude.Text)
createInstancesFromSnapshot_restoreDate = Lens.lens (\CreateInstancesFromSnapshot' {restoreDate} -> restoreDate) (\s@CreateInstancesFromSnapshot' {} a -> s {restoreDate = a} :: CreateInstancesFromSnapshot)

-- | You can create a launch script that configures a server with additional
-- user data. For example, @apt-get -y update@.
--
-- Depending on the machine image you choose, the command to get software
-- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
-- Ubuntu use @apt-get@, and FreeBSD uses @pkg@. For a complete list, see
-- the
-- <https://lightsail.aws.amazon.com/ls/docs/getting-started/article/compare-options-choose-lightsail-instance-image Dev Guide>.
createInstancesFromSnapshot_userData :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe Prelude.Text)
createInstancesFromSnapshot_userData = Lens.lens (\CreateInstancesFromSnapshot' {userData} -> userData) (\s@CreateInstancesFromSnapshot' {} a -> s {userData = a} :: CreateInstancesFromSnapshot)

-- | An array of objects representing the add-ons to enable for the new
-- instance.
createInstancesFromSnapshot_addOns :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe [AddOnRequest])
createInstancesFromSnapshot_addOns = Lens.lens (\CreateInstancesFromSnapshot' {addOns} -> addOns) (\s@CreateInstancesFromSnapshot' {} a -> s {addOns = a} :: CreateInstancesFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | An object containing information about one or more disk mappings.
createInstancesFromSnapshot_attachedDiskMapping :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe (Prelude.HashMap Prelude.Text [DiskMap]))
createInstancesFromSnapshot_attachedDiskMapping = Lens.lens (\CreateInstancesFromSnapshot' {attachedDiskMapping} -> attachedDiskMapping) (\s@CreateInstancesFromSnapshot' {} a -> s {attachedDiskMapping = a} :: CreateInstancesFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the instance snapshot on which you are basing your new
-- instances. Use the get instance snapshots operation to return
-- information about your existing snapshots.
--
-- Constraint:
--
-- -   This parameter cannot be defined together with the
--     @source instance name@ parameter. The @instance snapshot name@ and
--     @source instance name@ parameters are mutually exclusive.
createInstancesFromSnapshot_instanceSnapshotName :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe Prelude.Text)
createInstancesFromSnapshot_instanceSnapshotName = Lens.lens (\CreateInstancesFromSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@CreateInstancesFromSnapshot' {} a -> s {instanceSnapshotName = a} :: CreateInstancesFromSnapshot)

-- | The name for your key pair.
createInstancesFromSnapshot_keyPairName :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe Prelude.Text)
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
createInstancesFromSnapshot_useLatestRestorableAutoSnapshot :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe Prelude.Bool)
createInstancesFromSnapshot_useLatestRestorableAutoSnapshot = Lens.lens (\CreateInstancesFromSnapshot' {useLatestRestorableAutoSnapshot} -> useLatestRestorableAutoSnapshot) (\s@CreateInstancesFromSnapshot' {} a -> s {useLatestRestorableAutoSnapshot = a} :: CreateInstancesFromSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createInstancesFromSnapshot_tags :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe [Tag])
createInstancesFromSnapshot_tags = Lens.lens (\CreateInstancesFromSnapshot' {tags} -> tags) (\s@CreateInstancesFromSnapshot' {} a -> s {tags = a} :: CreateInstancesFromSnapshot) Prelude.. Lens.mapping Lens._Coerce

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
createInstancesFromSnapshot_sourceInstanceName :: Lens.Lens' CreateInstancesFromSnapshot (Prelude.Maybe Prelude.Text)
createInstancesFromSnapshot_sourceInstanceName = Lens.lens (\CreateInstancesFromSnapshot' {sourceInstanceName} -> sourceInstanceName) (\s@CreateInstancesFromSnapshot' {} a -> s {sourceInstanceName = a} :: CreateInstancesFromSnapshot)

-- | The names for your new instances.
createInstancesFromSnapshot_instanceNames :: Lens.Lens' CreateInstancesFromSnapshot [Prelude.Text]
createInstancesFromSnapshot_instanceNames = Lens.lens (\CreateInstancesFromSnapshot' {instanceNames} -> instanceNames) (\s@CreateInstancesFromSnapshot' {} a -> s {instanceNames = a} :: CreateInstancesFromSnapshot) Prelude.. Lens._Coerce

-- | The Availability Zone where you want to create your instances. Use the
-- following formatting: @us-east-2a@ (case sensitive). You can get a list
-- of Availability Zones by using the
-- <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions>
-- operation. Be sure to add the @include Availability Zones@ parameter to
-- your request.
createInstancesFromSnapshot_availabilityZone :: Lens.Lens' CreateInstancesFromSnapshot Prelude.Text
createInstancesFromSnapshot_availabilityZone = Lens.lens (\CreateInstancesFromSnapshot' {availabilityZone} -> availabilityZone) (\s@CreateInstancesFromSnapshot' {} a -> s {availabilityZone = a} :: CreateInstancesFromSnapshot)

-- | The bundle of specification information for your virtual private server
-- (or /instance/), including the pricing plan (e.g., @micro_1_0@).
createInstancesFromSnapshot_bundleId :: Lens.Lens' CreateInstancesFromSnapshot Prelude.Text
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
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstancesFromSnapshot

instance Prelude.NFData CreateInstancesFromSnapshot

instance Core.ToHeaders CreateInstancesFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateInstancesFromSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateInstancesFromSnapshot where
  toJSON CreateInstancesFromSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ipAddressType" Core..=) Prelude.<$> ipAddressType,
            ("restoreDate" Core..=) Prelude.<$> restoreDate,
            ("userData" Core..=) Prelude.<$> userData,
            ("addOns" Core..=) Prelude.<$> addOns,
            ("attachedDiskMapping" Core..=)
              Prelude.<$> attachedDiskMapping,
            ("instanceSnapshotName" Core..=)
              Prelude.<$> instanceSnapshotName,
            ("keyPairName" Core..=) Prelude.<$> keyPairName,
            ("useLatestRestorableAutoSnapshot" Core..=)
              Prelude.<$> useLatestRestorableAutoSnapshot,
            ("tags" Core..=) Prelude.<$> tags,
            ("sourceInstanceName" Core..=)
              Prelude.<$> sourceInstanceName,
            Prelude.Just ("instanceNames" Core..= instanceNames),
            Prelude.Just
              ("availabilityZone" Core..= availabilityZone),
            Prelude.Just ("bundleId" Core..= bundleId)
          ]
      )

instance Core.ToPath CreateInstancesFromSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateInstancesFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInstancesFromSnapshotResponse' smart constructor.
data CreateInstancesFromSnapshotResponse = CreateInstancesFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateInstancesFromSnapshotResponse
newCreateInstancesFromSnapshotResponse pHttpStatus_ =
  CreateInstancesFromSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createInstancesFromSnapshotResponse_operations :: Lens.Lens' CreateInstancesFromSnapshotResponse (Prelude.Maybe [Operation])
createInstancesFromSnapshotResponse_operations = Lens.lens (\CreateInstancesFromSnapshotResponse' {operations} -> operations) (\s@CreateInstancesFromSnapshotResponse' {} a -> s {operations = a} :: CreateInstancesFromSnapshotResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createInstancesFromSnapshotResponse_httpStatus :: Lens.Lens' CreateInstancesFromSnapshotResponse Prelude.Int
createInstancesFromSnapshotResponse_httpStatus = Lens.lens (\CreateInstancesFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateInstancesFromSnapshotResponse' {} a -> s {httpStatus = a} :: CreateInstancesFromSnapshotResponse)

instance
  Prelude.NFData
    CreateInstancesFromSnapshotResponse
