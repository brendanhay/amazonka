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
-- Module      : Amazonka.SSM.DescribePatchGroupState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns high-level aggregated patch compliance state information for a
-- patch group.
module Amazonka.SSM.DescribePatchGroupState
  ( -- * Creating a Request
    DescribePatchGroupState (..),
    newDescribePatchGroupState,

    -- * Request Lenses
    describePatchGroupState_patchGroup,

    -- * Destructuring the Response
    DescribePatchGroupStateResponse (..),
    newDescribePatchGroupStateResponse,

    -- * Response Lenses
    describePatchGroupStateResponse_instances,
    describePatchGroupStateResponse_instancesWithInstalledPatches,
    describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithInstalledOtherPatches,
    describePatchGroupStateResponse_instancesWithSecurityNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithFailedPatches,
    describePatchGroupStateResponse_instancesWithOtherNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithMissingPatches,
    describePatchGroupStateResponse_instancesWithInstalledRejectedPatches,
    describePatchGroupStateResponse_instancesWithCriticalNonCompliantPatches,
    describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches,
    describePatchGroupStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribePatchGroupState' smart constructor.
data DescribePatchGroupState = DescribePatchGroupState'
  { -- | The name of the patch group whose patch snapshot should be retrieved.
    patchGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchGroupState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchGroup', 'describePatchGroupState_patchGroup' - The name of the patch group whose patch snapshot should be retrieved.
newDescribePatchGroupState ::
  -- | 'patchGroup'
  Prelude.Text ->
  DescribePatchGroupState
newDescribePatchGroupState pPatchGroup_ =
  DescribePatchGroupState' {patchGroup = pPatchGroup_}

-- | The name of the patch group whose patch snapshot should be retrieved.
describePatchGroupState_patchGroup :: Lens.Lens' DescribePatchGroupState Prelude.Text
describePatchGroupState_patchGroup = Lens.lens (\DescribePatchGroupState' {patchGroup} -> patchGroup) (\s@DescribePatchGroupState' {} a -> s {patchGroup = a} :: DescribePatchGroupState)

instance Core.AWSRequest DescribePatchGroupState where
  type
    AWSResponse DescribePatchGroupState =
      DescribePatchGroupStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchGroupStateResponse'
            Prelude.<$> (x Core..?> "Instances")
            Prelude.<*> (x Core..?> "InstancesWithInstalledPatches")
            Prelude.<*> ( x
                            Core..?> "InstancesWithUnreportedNotApplicablePatches"
                        )
            Prelude.<*> (x Core..?> "InstancesWithInstalledOtherPatches")
            Prelude.<*> ( x
                            Core..?> "InstancesWithSecurityNonCompliantPatches"
                        )
            Prelude.<*> (x Core..?> "InstancesWithFailedPatches")
            Prelude.<*> (x Core..?> "InstancesWithOtherNonCompliantPatches")
            Prelude.<*> (x Core..?> "InstancesWithNotApplicablePatches")
            Prelude.<*> (x Core..?> "InstancesWithMissingPatches")
            Prelude.<*> (x Core..?> "InstancesWithInstalledRejectedPatches")
            Prelude.<*> ( x
                            Core..?> "InstancesWithCriticalNonCompliantPatches"
                        )
            Prelude.<*> ( x
                            Core..?> "InstancesWithInstalledPendingRebootPatches"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePatchGroupState where
  hashWithSalt _salt DescribePatchGroupState' {..} =
    _salt `Prelude.hashWithSalt` patchGroup

instance Prelude.NFData DescribePatchGroupState where
  rnf DescribePatchGroupState' {..} =
    Prelude.rnf patchGroup

instance Core.ToHeaders DescribePatchGroupState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribePatchGroupState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePatchGroupState where
  toJSON DescribePatchGroupState' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("PatchGroup" Core..= patchGroup)]
      )

instance Core.ToPath DescribePatchGroupState where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePatchGroupState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePatchGroupStateResponse' smart constructor.
data DescribePatchGroupStateResponse = DescribePatchGroupStateResponse'
  { -- | The number of managed nodes in the patch group.
    instances :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with installed patches.
    instancesWithInstalledPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with @NotApplicable@ patches beyond the
    -- supported limit, which aren\'t reported by name to Inventory. Inventory
    -- is a capability of Amazon Web Services Systems Manager.
    instancesWithUnreportedNotApplicablePatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with patches installed that aren\'t defined
    -- in the patch baseline.
    instancesWithInstalledOtherPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes where patches that are specified as
    -- @Security@ in a patch advisory aren\'t installed. These patches might be
    -- missing, have failed installation, were rejected, or were installed but
    -- awaiting a required managed node reboot. The status of these managed
    -- nodes is @NON_COMPLIANT@.
    instancesWithSecurityNonCompliantPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with patches from the patch baseline that
    -- failed to install.
    instancesWithFailedPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with patches installed that are specified as
    -- other than @Critical@ or @Security@ but aren\'t compliant with the patch
    -- baseline. The status of these managed nodes is @NON_COMPLIANT@.
    instancesWithOtherNonCompliantPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with patches that aren\'t applicable.
    instancesWithNotApplicablePatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with missing patches from the patch
    -- baseline.
    instancesWithMissingPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with patches installed that are specified in
    -- a @RejectedPatches@ list. Patches with a status of @INSTALLED_REJECTED@
    -- were typically installed before they were added to a @RejectedPatches@
    -- list.
    --
    -- If @ALLOW_AS_DEPENDENCY@ is the specified option for
    -- @RejectedPatchesAction@, the value of
    -- @InstancesWithInstalledRejectedPatches@ will always be @0@ (zero).
    instancesWithInstalledRejectedPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes where patches that are specified as
    -- @Critical@ for compliance reporting in the patch baseline aren\'t
    -- installed. These patches might be missing, have failed installation,
    -- were rejected, or were installed but awaiting a required managed node
    -- reboot. The status of these managed nodes is @NON_COMPLIANT@.
    instancesWithCriticalNonCompliantPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of managed nodes with patches installed by Patch Manager that
    -- haven\'t been rebooted after the patch installation. The status of these
    -- managed nodes is @NON_COMPLIANT@.
    instancesWithInstalledPendingRebootPatches :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchGroupStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describePatchGroupStateResponse_instances' - The number of managed nodes in the patch group.
--
-- 'instancesWithInstalledPatches', 'describePatchGroupStateResponse_instancesWithInstalledPatches' - The number of managed nodes with installed patches.
--
-- 'instancesWithUnreportedNotApplicablePatches', 'describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches' - The number of managed nodes with @NotApplicable@ patches beyond the
-- supported limit, which aren\'t reported by name to Inventory. Inventory
-- is a capability of Amazon Web Services Systems Manager.
--
-- 'instancesWithInstalledOtherPatches', 'describePatchGroupStateResponse_instancesWithInstalledOtherPatches' - The number of managed nodes with patches installed that aren\'t defined
-- in the patch baseline.
--
-- 'instancesWithSecurityNonCompliantPatches', 'describePatchGroupStateResponse_instancesWithSecurityNonCompliantPatches' - The number of managed nodes where patches that are specified as
-- @Security@ in a patch advisory aren\'t installed. These patches might be
-- missing, have failed installation, were rejected, or were installed but
-- awaiting a required managed node reboot. The status of these managed
-- nodes is @NON_COMPLIANT@.
--
-- 'instancesWithFailedPatches', 'describePatchGroupStateResponse_instancesWithFailedPatches' - The number of managed nodes with patches from the patch baseline that
-- failed to install.
--
-- 'instancesWithOtherNonCompliantPatches', 'describePatchGroupStateResponse_instancesWithOtherNonCompliantPatches' - The number of managed nodes with patches installed that are specified as
-- other than @Critical@ or @Security@ but aren\'t compliant with the patch
-- baseline. The status of these managed nodes is @NON_COMPLIANT@.
--
-- 'instancesWithNotApplicablePatches', 'describePatchGroupStateResponse_instancesWithNotApplicablePatches' - The number of managed nodes with patches that aren\'t applicable.
--
-- 'instancesWithMissingPatches', 'describePatchGroupStateResponse_instancesWithMissingPatches' - The number of managed nodes with missing patches from the patch
-- baseline.
--
-- 'instancesWithInstalledRejectedPatches', 'describePatchGroupStateResponse_instancesWithInstalledRejectedPatches' - The number of managed nodes with patches installed that are specified in
-- a @RejectedPatches@ list. Patches with a status of @INSTALLED_REJECTED@
-- were typically installed before they were added to a @RejectedPatches@
-- list.
--
-- If @ALLOW_AS_DEPENDENCY@ is the specified option for
-- @RejectedPatchesAction@, the value of
-- @InstancesWithInstalledRejectedPatches@ will always be @0@ (zero).
--
-- 'instancesWithCriticalNonCompliantPatches', 'describePatchGroupStateResponse_instancesWithCriticalNonCompliantPatches' - The number of managed nodes where patches that are specified as
-- @Critical@ for compliance reporting in the patch baseline aren\'t
-- installed. These patches might be missing, have failed installation,
-- were rejected, or were installed but awaiting a required managed node
-- reboot. The status of these managed nodes is @NON_COMPLIANT@.
--
-- 'instancesWithInstalledPendingRebootPatches', 'describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches' - The number of managed nodes with patches installed by Patch Manager that
-- haven\'t been rebooted after the patch installation. The status of these
-- managed nodes is @NON_COMPLIANT@.
--
-- 'httpStatus', 'describePatchGroupStateResponse_httpStatus' - The response's http status code.
newDescribePatchGroupStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePatchGroupStateResponse
newDescribePatchGroupStateResponse pHttpStatus_ =
  DescribePatchGroupStateResponse'
    { instances =
        Prelude.Nothing,
      instancesWithInstalledPatches =
        Prelude.Nothing,
      instancesWithUnreportedNotApplicablePatches =
        Prelude.Nothing,
      instancesWithInstalledOtherPatches =
        Prelude.Nothing,
      instancesWithSecurityNonCompliantPatches =
        Prelude.Nothing,
      instancesWithFailedPatches =
        Prelude.Nothing,
      instancesWithOtherNonCompliantPatches =
        Prelude.Nothing,
      instancesWithNotApplicablePatches =
        Prelude.Nothing,
      instancesWithMissingPatches =
        Prelude.Nothing,
      instancesWithInstalledRejectedPatches =
        Prelude.Nothing,
      instancesWithCriticalNonCompliantPatches =
        Prelude.Nothing,
      instancesWithInstalledPendingRebootPatches =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of managed nodes in the patch group.
describePatchGroupStateResponse_instances :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instances = Lens.lens (\DescribePatchGroupStateResponse' {instances} -> instances) (\s@DescribePatchGroupStateResponse' {} a -> s {instances = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with installed patches.
describePatchGroupStateResponse_instancesWithInstalledPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledPatches} -> instancesWithInstalledPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with @NotApplicable@ patches beyond the
-- supported limit, which aren\'t reported by name to Inventory. Inventory
-- is a capability of Amazon Web Services Systems Manager.
describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithUnreportedNotApplicablePatches} -> instancesWithUnreportedNotApplicablePatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithUnreportedNotApplicablePatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with patches installed that aren\'t defined
-- in the patch baseline.
describePatchGroupStateResponse_instancesWithInstalledOtherPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledOtherPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledOtherPatches} -> instancesWithInstalledOtherPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledOtherPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes where patches that are specified as
-- @Security@ in a patch advisory aren\'t installed. These patches might be
-- missing, have failed installation, were rejected, or were installed but
-- awaiting a required managed node reboot. The status of these managed
-- nodes is @NON_COMPLIANT@.
describePatchGroupStateResponse_instancesWithSecurityNonCompliantPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithSecurityNonCompliantPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithSecurityNonCompliantPatches} -> instancesWithSecurityNonCompliantPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithSecurityNonCompliantPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with patches from the patch baseline that
-- failed to install.
describePatchGroupStateResponse_instancesWithFailedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithFailedPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithFailedPatches} -> instancesWithFailedPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithFailedPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with patches installed that are specified as
-- other than @Critical@ or @Security@ but aren\'t compliant with the patch
-- baseline. The status of these managed nodes is @NON_COMPLIANT@.
describePatchGroupStateResponse_instancesWithOtherNonCompliantPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithOtherNonCompliantPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithOtherNonCompliantPatches} -> instancesWithOtherNonCompliantPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithOtherNonCompliantPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with patches that aren\'t applicable.
describePatchGroupStateResponse_instancesWithNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithNotApplicablePatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithNotApplicablePatches} -> instancesWithNotApplicablePatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithNotApplicablePatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with missing patches from the patch
-- baseline.
describePatchGroupStateResponse_instancesWithMissingPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithMissingPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithMissingPatches} -> instancesWithMissingPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithMissingPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with patches installed that are specified in
-- a @RejectedPatches@ list. Patches with a status of @INSTALLED_REJECTED@
-- were typically installed before they were added to a @RejectedPatches@
-- list.
--
-- If @ALLOW_AS_DEPENDENCY@ is the specified option for
-- @RejectedPatchesAction@, the value of
-- @InstancesWithInstalledRejectedPatches@ will always be @0@ (zero).
describePatchGroupStateResponse_instancesWithInstalledRejectedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledRejectedPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledRejectedPatches} -> instancesWithInstalledRejectedPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledRejectedPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes where patches that are specified as
-- @Critical@ for compliance reporting in the patch baseline aren\'t
-- installed. These patches might be missing, have failed installation,
-- were rejected, or were installed but awaiting a required managed node
-- reboot. The status of these managed nodes is @NON_COMPLIANT@.
describePatchGroupStateResponse_instancesWithCriticalNonCompliantPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithCriticalNonCompliantPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithCriticalNonCompliantPatches} -> instancesWithCriticalNonCompliantPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithCriticalNonCompliantPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of managed nodes with patches installed by Patch Manager that
-- haven\'t been rebooted after the patch installation. The status of these
-- managed nodes is @NON_COMPLIANT@.
describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledPendingRebootPatches} -> instancesWithInstalledPendingRebootPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledPendingRebootPatches = a} :: DescribePatchGroupStateResponse)

-- | The response's http status code.
describePatchGroupStateResponse_httpStatus :: Lens.Lens' DescribePatchGroupStateResponse Prelude.Int
describePatchGroupStateResponse_httpStatus = Lens.lens (\DescribePatchGroupStateResponse' {httpStatus} -> httpStatus) (\s@DescribePatchGroupStateResponse' {} a -> s {httpStatus = a} :: DescribePatchGroupStateResponse)

instance
  Prelude.NFData
    DescribePatchGroupStateResponse
  where
  rnf DescribePatchGroupStateResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf instancesWithInstalledPatches
      `Prelude.seq` Prelude.rnf
        instancesWithUnreportedNotApplicablePatches
      `Prelude.seq` Prelude.rnf instancesWithInstalledOtherPatches
      `Prelude.seq` Prelude.rnf instancesWithSecurityNonCompliantPatches
      `Prelude.seq` Prelude.rnf instancesWithFailedPatches
      `Prelude.seq` Prelude.rnf instancesWithOtherNonCompliantPatches
      `Prelude.seq` Prelude.rnf instancesWithNotApplicablePatches
      `Prelude.seq` Prelude.rnf instancesWithMissingPatches
      `Prelude.seq` Prelude.rnf instancesWithInstalledRejectedPatches
      `Prelude.seq` Prelude.rnf
        instancesWithCriticalNonCompliantPatches
      `Prelude.seq` Prelude.rnf
        instancesWithInstalledPendingRebootPatches
      `Prelude.seq` Prelude.rnf httpStatus
