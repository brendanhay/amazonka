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
-- Module      : Network.AWS.EC2.RestoreManagedPrefixListVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the entries from a previous version of a managed prefix list to
-- a new version of the prefix list.
module Network.AWS.EC2.RestoreManagedPrefixListVersion
  ( -- * Creating a Request
    RestoreManagedPrefixListVersion (..),
    newRestoreManagedPrefixListVersion,

    -- * Request Lenses
    restoreManagedPrefixListVersion_dryRun,
    restoreManagedPrefixListVersion_prefixListId,
    restoreManagedPrefixListVersion_previousVersion,
    restoreManagedPrefixListVersion_currentVersion,

    -- * Destructuring the Response
    RestoreManagedPrefixListVersionResponse (..),
    newRestoreManagedPrefixListVersionResponse,

    -- * Response Lenses
    restoreManagedPrefixListVersionResponse_prefixList,
    restoreManagedPrefixListVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreManagedPrefixListVersion' smart constructor.
data RestoreManagedPrefixListVersion = RestoreManagedPrefixListVersion'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Core.Text,
    -- | The version to restore.
    previousVersion :: Core.Integer,
    -- | The current version number for the prefix list.
    currentVersion :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreManagedPrefixListVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'restoreManagedPrefixListVersion_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'prefixListId', 'restoreManagedPrefixListVersion_prefixListId' - The ID of the prefix list.
--
-- 'previousVersion', 'restoreManagedPrefixListVersion_previousVersion' - The version to restore.
--
-- 'currentVersion', 'restoreManagedPrefixListVersion_currentVersion' - The current version number for the prefix list.
newRestoreManagedPrefixListVersion ::
  -- | 'prefixListId'
  Core.Text ->
  -- | 'previousVersion'
  Core.Integer ->
  -- | 'currentVersion'
  Core.Integer ->
  RestoreManagedPrefixListVersion
newRestoreManagedPrefixListVersion
  pPrefixListId_
  pPreviousVersion_
  pCurrentVersion_ =
    RestoreManagedPrefixListVersion'
      { dryRun =
          Core.Nothing,
        prefixListId = pPrefixListId_,
        previousVersion = pPreviousVersion_,
        currentVersion = pCurrentVersion_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreManagedPrefixListVersion_dryRun :: Lens.Lens' RestoreManagedPrefixListVersion (Core.Maybe Core.Bool)
restoreManagedPrefixListVersion_dryRun = Lens.lens (\RestoreManagedPrefixListVersion' {dryRun} -> dryRun) (\s@RestoreManagedPrefixListVersion' {} a -> s {dryRun = a} :: RestoreManagedPrefixListVersion)

-- | The ID of the prefix list.
restoreManagedPrefixListVersion_prefixListId :: Lens.Lens' RestoreManagedPrefixListVersion Core.Text
restoreManagedPrefixListVersion_prefixListId = Lens.lens (\RestoreManagedPrefixListVersion' {prefixListId} -> prefixListId) (\s@RestoreManagedPrefixListVersion' {} a -> s {prefixListId = a} :: RestoreManagedPrefixListVersion)

-- | The version to restore.
restoreManagedPrefixListVersion_previousVersion :: Lens.Lens' RestoreManagedPrefixListVersion Core.Integer
restoreManagedPrefixListVersion_previousVersion = Lens.lens (\RestoreManagedPrefixListVersion' {previousVersion} -> previousVersion) (\s@RestoreManagedPrefixListVersion' {} a -> s {previousVersion = a} :: RestoreManagedPrefixListVersion)

-- | The current version number for the prefix list.
restoreManagedPrefixListVersion_currentVersion :: Lens.Lens' RestoreManagedPrefixListVersion Core.Integer
restoreManagedPrefixListVersion_currentVersion = Lens.lens (\RestoreManagedPrefixListVersion' {currentVersion} -> currentVersion) (\s@RestoreManagedPrefixListVersion' {} a -> s {currentVersion = a} :: RestoreManagedPrefixListVersion)

instance
  Core.AWSRequest
    RestoreManagedPrefixListVersion
  where
  type
    AWSResponse RestoreManagedPrefixListVersion =
      RestoreManagedPrefixListVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreManagedPrefixListVersionResponse'
            Core.<$> (x Core..@? "prefixList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RestoreManagedPrefixListVersion

instance Core.NFData RestoreManagedPrefixListVersion

instance
  Core.ToHeaders
    RestoreManagedPrefixListVersion
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RestoreManagedPrefixListVersion where
  toPath = Core.const "/"

instance Core.ToQuery RestoreManagedPrefixListVersion where
  toQuery RestoreManagedPrefixListVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RestoreManagedPrefixListVersion" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "PrefixListId" Core.=: prefixListId,
        "PreviousVersion" Core.=: previousVersion,
        "CurrentVersion" Core.=: currentVersion
      ]

-- | /See:/ 'newRestoreManagedPrefixListVersionResponse' smart constructor.
data RestoreManagedPrefixListVersionResponse = RestoreManagedPrefixListVersionResponse'
  { -- | Information about the prefix list.
    prefixList :: Core.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreManagedPrefixListVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixList', 'restoreManagedPrefixListVersionResponse_prefixList' - Information about the prefix list.
--
-- 'httpStatus', 'restoreManagedPrefixListVersionResponse_httpStatus' - The response's http status code.
newRestoreManagedPrefixListVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RestoreManagedPrefixListVersionResponse
newRestoreManagedPrefixListVersionResponse
  pHttpStatus_ =
    RestoreManagedPrefixListVersionResponse'
      { prefixList =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the prefix list.
restoreManagedPrefixListVersionResponse_prefixList :: Lens.Lens' RestoreManagedPrefixListVersionResponse (Core.Maybe ManagedPrefixList)
restoreManagedPrefixListVersionResponse_prefixList = Lens.lens (\RestoreManagedPrefixListVersionResponse' {prefixList} -> prefixList) (\s@RestoreManagedPrefixListVersionResponse' {} a -> s {prefixList = a} :: RestoreManagedPrefixListVersionResponse)

-- | The response's http status code.
restoreManagedPrefixListVersionResponse_httpStatus :: Lens.Lens' RestoreManagedPrefixListVersionResponse Core.Int
restoreManagedPrefixListVersionResponse_httpStatus = Lens.lens (\RestoreManagedPrefixListVersionResponse' {httpStatus} -> httpStatus) (\s@RestoreManagedPrefixListVersionResponse' {} a -> s {httpStatus = a} :: RestoreManagedPrefixListVersionResponse)

instance
  Core.NFData
    RestoreManagedPrefixListVersionResponse
