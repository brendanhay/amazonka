{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.DescribePatchGroupState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns high-level aggregated patch compliance state for a patch group.
module Network.AWS.SSM.DescribePatchGroupState
  ( -- * Creating a Request
    DescribePatchGroupState (..),
    newDescribePatchGroupState,

    -- * Request Lenses
    describePatchGroupState_patchGroup,

    -- * Destructuring the Response
    DescribePatchGroupStateResponse (..),
    newDescribePatchGroupStateResponse,

    -- * Response Lenses
    describePatchGroupStateResponse_instancesWithInstalledOtherPatches,
    describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches,
    describePatchGroupStateResponse_instancesWithInstalledRejectedPatches,
    describePatchGroupStateResponse_instances,
    describePatchGroupStateResponse_instancesWithMissingPatches,
    describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches,
    describePatchGroupStateResponse_instancesWithFailedPatches,
    describePatchGroupStateResponse_instancesWithInstalledPatches,
    describePatchGroupStateResponse_instancesWithNotApplicablePatches,
    describePatchGroupStateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribePatchGroupState' smart constructor.
data DescribePatchGroupState = DescribePatchGroupState'
  { -- | The name of the patch group whose patch snapshot should be retrieved.
    patchGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribePatchGroupState where
  type
    Rs DescribePatchGroupState =
      DescribePatchGroupStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchGroupStateResponse'
            Prelude.<$> (x Prelude..?> "InstancesWithInstalledOtherPatches")
            Prelude.<*> ( x
                            Prelude..?> "InstancesWithUnreportedNotApplicablePatches"
                        )
            Prelude.<*> ( x
                            Prelude..?> "InstancesWithInstalledRejectedPatches"
                        )
            Prelude.<*> (x Prelude..?> "Instances")
            Prelude.<*> (x Prelude..?> "InstancesWithMissingPatches")
            Prelude.<*> ( x
                            Prelude..?> "InstancesWithInstalledPendingRebootPatches"
                        )
            Prelude.<*> (x Prelude..?> "InstancesWithFailedPatches")
            Prelude.<*> (x Prelude..?> "InstancesWithInstalledPatches")
            Prelude.<*> (x Prelude..?> "InstancesWithNotApplicablePatches")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePatchGroupState

instance Prelude.NFData DescribePatchGroupState

instance Prelude.ToHeaders DescribePatchGroupState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DescribePatchGroupState" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribePatchGroupState where
  toJSON DescribePatchGroupState' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("PatchGroup" Prelude..= patchGroup)]
      )

instance Prelude.ToPath DescribePatchGroupState where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribePatchGroupState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePatchGroupStateResponse' smart constructor.
data DescribePatchGroupStateResponse = DescribePatchGroupStateResponse'
  { -- | The number of instances with patches installed that aren\'t defined in
    -- the patch baseline.
    instancesWithInstalledOtherPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with @NotApplicable@ patches beyond the
    -- supported limit, which are not reported by name to Systems Manager
    -- Inventory.
    instancesWithUnreportedNotApplicablePatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with patches installed that are specified in a
    -- RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were
    -- typically installed before they were added to a RejectedPatches list.
    --
    -- If ALLOW_AS_DEPENDENCY is the specified option for
    -- RejectedPatchesAction, the value of
    -- InstancesWithInstalledRejectedPatches will always be 0 (zero).
    instancesWithInstalledRejectedPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in the patch group.
    instances :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with missing patches from the patch baseline.
    instancesWithMissingPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with patches installed by Patch Manager that
    -- have not been rebooted after the patch installation. The status of these
    -- instances is NON_COMPLIANT.
    instancesWithInstalledPendingRebootPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with patches from the patch baseline that failed
    -- to install.
    instancesWithFailedPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with installed patches.
    instancesWithInstalledPatches :: Prelude.Maybe Prelude.Int,
    -- | The number of instances with patches that aren\'t applicable.
    instancesWithNotApplicablePatches :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribePatchGroupStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesWithInstalledOtherPatches', 'describePatchGroupStateResponse_instancesWithInstalledOtherPatches' - The number of instances with patches installed that aren\'t defined in
-- the patch baseline.
--
-- 'instancesWithUnreportedNotApplicablePatches', 'describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches' - The number of instances with @NotApplicable@ patches beyond the
-- supported limit, which are not reported by name to Systems Manager
-- Inventory.
--
-- 'instancesWithInstalledRejectedPatches', 'describePatchGroupStateResponse_instancesWithInstalledRejectedPatches' - The number of instances with patches installed that are specified in a
-- RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were
-- typically installed before they were added to a RejectedPatches list.
--
-- If ALLOW_AS_DEPENDENCY is the specified option for
-- RejectedPatchesAction, the value of
-- InstancesWithInstalledRejectedPatches will always be 0 (zero).
--
-- 'instances', 'describePatchGroupStateResponse_instances' - The number of instances in the patch group.
--
-- 'instancesWithMissingPatches', 'describePatchGroupStateResponse_instancesWithMissingPatches' - The number of instances with missing patches from the patch baseline.
--
-- 'instancesWithInstalledPendingRebootPatches', 'describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches' - The number of instances with patches installed by Patch Manager that
-- have not been rebooted after the patch installation. The status of these
-- instances is NON_COMPLIANT.
--
-- 'instancesWithFailedPatches', 'describePatchGroupStateResponse_instancesWithFailedPatches' - The number of instances with patches from the patch baseline that failed
-- to install.
--
-- 'instancesWithInstalledPatches', 'describePatchGroupStateResponse_instancesWithInstalledPatches' - The number of instances with installed patches.
--
-- 'instancesWithNotApplicablePatches', 'describePatchGroupStateResponse_instancesWithNotApplicablePatches' - The number of instances with patches that aren\'t applicable.
--
-- 'httpStatus', 'describePatchGroupStateResponse_httpStatus' - The response's http status code.
newDescribePatchGroupStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePatchGroupStateResponse
newDescribePatchGroupStateResponse pHttpStatus_ =
  DescribePatchGroupStateResponse'
    { instancesWithInstalledOtherPatches =
        Prelude.Nothing,
      instancesWithUnreportedNotApplicablePatches =
        Prelude.Nothing,
      instancesWithInstalledRejectedPatches =
        Prelude.Nothing,
      instances = Prelude.Nothing,
      instancesWithMissingPatches =
        Prelude.Nothing,
      instancesWithInstalledPendingRebootPatches =
        Prelude.Nothing,
      instancesWithFailedPatches =
        Prelude.Nothing,
      instancesWithInstalledPatches =
        Prelude.Nothing,
      instancesWithNotApplicablePatches =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of instances with patches installed that aren\'t defined in
-- the patch baseline.
describePatchGroupStateResponse_instancesWithInstalledOtherPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledOtherPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledOtherPatches} -> instancesWithInstalledOtherPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledOtherPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with @NotApplicable@ patches beyond the
-- supported limit, which are not reported by name to Systems Manager
-- Inventory.
describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithUnreportedNotApplicablePatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithUnreportedNotApplicablePatches} -> instancesWithUnreportedNotApplicablePatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithUnreportedNotApplicablePatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with patches installed that are specified in a
-- RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were
-- typically installed before they were added to a RejectedPatches list.
--
-- If ALLOW_AS_DEPENDENCY is the specified option for
-- RejectedPatchesAction, the value of
-- InstancesWithInstalledRejectedPatches will always be 0 (zero).
describePatchGroupStateResponse_instancesWithInstalledRejectedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledRejectedPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledRejectedPatches} -> instancesWithInstalledRejectedPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledRejectedPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances in the patch group.
describePatchGroupStateResponse_instances :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instances = Lens.lens (\DescribePatchGroupStateResponse' {instances} -> instances) (\s@DescribePatchGroupStateResponse' {} a -> s {instances = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with missing patches from the patch baseline.
describePatchGroupStateResponse_instancesWithMissingPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithMissingPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithMissingPatches} -> instancesWithMissingPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithMissingPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with patches installed by Patch Manager that
-- have not been rebooted after the patch installation. The status of these
-- instances is NON_COMPLIANT.
describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledPendingRebootPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledPendingRebootPatches} -> instancesWithInstalledPendingRebootPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledPendingRebootPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with patches from the patch baseline that failed
-- to install.
describePatchGroupStateResponse_instancesWithFailedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithFailedPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithFailedPatches} -> instancesWithFailedPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithFailedPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with installed patches.
describePatchGroupStateResponse_instancesWithInstalledPatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithInstalledPatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithInstalledPatches} -> instancesWithInstalledPatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithInstalledPatches = a} :: DescribePatchGroupStateResponse)

-- | The number of instances with patches that aren\'t applicable.
describePatchGroupStateResponse_instancesWithNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Prelude.Maybe Prelude.Int)
describePatchGroupStateResponse_instancesWithNotApplicablePatches = Lens.lens (\DescribePatchGroupStateResponse' {instancesWithNotApplicablePatches} -> instancesWithNotApplicablePatches) (\s@DescribePatchGroupStateResponse' {} a -> s {instancesWithNotApplicablePatches = a} :: DescribePatchGroupStateResponse)

-- | The response's http status code.
describePatchGroupStateResponse_httpStatus :: Lens.Lens' DescribePatchGroupStateResponse Prelude.Int
describePatchGroupStateResponse_httpStatus = Lens.lens (\DescribePatchGroupStateResponse' {httpStatus} -> httpStatus) (\s@DescribePatchGroupStateResponse' {} a -> s {httpStatus = a} :: DescribePatchGroupStateResponse)

instance
  Prelude.NFData
    DescribePatchGroupStateResponse
