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
-- Module      : Amazonka.EC2.RestoreManagedPrefixListVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the entries from a previous version of a managed prefix list to
-- a new version of the prefix list.
module Amazonka.EC2.RestoreManagedPrefixListVersion
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreManagedPrefixListVersion' smart constructor.
data RestoreManagedPrefixListVersion = RestoreManagedPrefixListVersion'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text,
    -- | The version to restore.
    previousVersion :: Prelude.Integer,
    -- | The current version number for the prefix list.
    currentVersion :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'previousVersion'
  Prelude.Integer ->
  -- | 'currentVersion'
  Prelude.Integer ->
  RestoreManagedPrefixListVersion
newRestoreManagedPrefixListVersion
  pPrefixListId_
  pPreviousVersion_
  pCurrentVersion_ =
    RestoreManagedPrefixListVersion'
      { dryRun =
          Prelude.Nothing,
        prefixListId = pPrefixListId_,
        previousVersion = pPreviousVersion_,
        currentVersion = pCurrentVersion_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreManagedPrefixListVersion_dryRun :: Lens.Lens' RestoreManagedPrefixListVersion (Prelude.Maybe Prelude.Bool)
restoreManagedPrefixListVersion_dryRun = Lens.lens (\RestoreManagedPrefixListVersion' {dryRun} -> dryRun) (\s@RestoreManagedPrefixListVersion' {} a -> s {dryRun = a} :: RestoreManagedPrefixListVersion)

-- | The ID of the prefix list.
restoreManagedPrefixListVersion_prefixListId :: Lens.Lens' RestoreManagedPrefixListVersion Prelude.Text
restoreManagedPrefixListVersion_prefixListId = Lens.lens (\RestoreManagedPrefixListVersion' {prefixListId} -> prefixListId) (\s@RestoreManagedPrefixListVersion' {} a -> s {prefixListId = a} :: RestoreManagedPrefixListVersion)

-- | The version to restore.
restoreManagedPrefixListVersion_previousVersion :: Lens.Lens' RestoreManagedPrefixListVersion Prelude.Integer
restoreManagedPrefixListVersion_previousVersion = Lens.lens (\RestoreManagedPrefixListVersion' {previousVersion} -> previousVersion) (\s@RestoreManagedPrefixListVersion' {} a -> s {previousVersion = a} :: RestoreManagedPrefixListVersion)

-- | The current version number for the prefix list.
restoreManagedPrefixListVersion_currentVersion :: Lens.Lens' RestoreManagedPrefixListVersion Prelude.Integer
restoreManagedPrefixListVersion_currentVersion = Lens.lens (\RestoreManagedPrefixListVersion' {currentVersion} -> currentVersion) (\s@RestoreManagedPrefixListVersion' {} a -> s {currentVersion = a} :: RestoreManagedPrefixListVersion)

instance
  Core.AWSRequest
    RestoreManagedPrefixListVersion
  where
  type
    AWSResponse RestoreManagedPrefixListVersion =
      RestoreManagedPrefixListVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreManagedPrefixListVersionResponse'
            Prelude.<$> (x Data..@? "prefixList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreManagedPrefixListVersion
  where
  hashWithSalt
    _salt
    RestoreManagedPrefixListVersion' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` prefixListId
        `Prelude.hashWithSalt` previousVersion
        `Prelude.hashWithSalt` currentVersion

instance
  Prelude.NFData
    RestoreManagedPrefixListVersion
  where
  rnf RestoreManagedPrefixListVersion' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf prefixListId `Prelude.seq`
        Prelude.rnf previousVersion `Prelude.seq`
          Prelude.rnf currentVersion

instance
  Data.ToHeaders
    RestoreManagedPrefixListVersion
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreManagedPrefixListVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreManagedPrefixListVersion where
  toQuery RestoreManagedPrefixListVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreManagedPrefixListVersion" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PrefixListId" Data.=: prefixListId,
        "PreviousVersion" Data.=: previousVersion,
        "CurrentVersion" Data.=: currentVersion
      ]

-- | /See:/ 'newRestoreManagedPrefixListVersionResponse' smart constructor.
data RestoreManagedPrefixListVersionResponse = RestoreManagedPrefixListVersionResponse'
  { -- | Information about the prefix list.
    prefixList :: Prelude.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RestoreManagedPrefixListVersionResponse
newRestoreManagedPrefixListVersionResponse
  pHttpStatus_ =
    RestoreManagedPrefixListVersionResponse'
      { prefixList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the prefix list.
restoreManagedPrefixListVersionResponse_prefixList :: Lens.Lens' RestoreManagedPrefixListVersionResponse (Prelude.Maybe ManagedPrefixList)
restoreManagedPrefixListVersionResponse_prefixList = Lens.lens (\RestoreManagedPrefixListVersionResponse' {prefixList} -> prefixList) (\s@RestoreManagedPrefixListVersionResponse' {} a -> s {prefixList = a} :: RestoreManagedPrefixListVersionResponse)

-- | The response's http status code.
restoreManagedPrefixListVersionResponse_httpStatus :: Lens.Lens' RestoreManagedPrefixListVersionResponse Prelude.Int
restoreManagedPrefixListVersionResponse_httpStatus = Lens.lens (\RestoreManagedPrefixListVersionResponse' {httpStatus} -> httpStatus) (\s@RestoreManagedPrefixListVersionResponse' {} a -> s {httpStatus = a} :: RestoreManagedPrefixListVersionResponse)

instance
  Prelude.NFData
    RestoreManagedPrefixListVersionResponse
  where
  rnf RestoreManagedPrefixListVersionResponse' {..} =
    Prelude.rnf prefixList `Prelude.seq`
      Prelude.rnf httpStatus
