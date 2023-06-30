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
-- Module      : Amazonka.SageMaker.DescribeSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the space.
module Amazonka.SageMaker.DescribeSpace
  ( -- * Creating a Request
    DescribeSpace (..),
    newDescribeSpace,

    -- * Request Lenses
    describeSpace_domainId,
    describeSpace_spaceName,

    -- * Destructuring the Response
    DescribeSpaceResponse (..),
    newDescribeSpaceResponse,

    -- * Response Lenses
    describeSpaceResponse_creationTime,
    describeSpaceResponse_domainId,
    describeSpaceResponse_failureReason,
    describeSpaceResponse_homeEfsFileSystemUid,
    describeSpaceResponse_lastModifiedTime,
    describeSpaceResponse_spaceArn,
    describeSpaceResponse_spaceName,
    describeSpaceResponse_spaceSettings,
    describeSpaceResponse_status,
    describeSpaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeSpace' smart constructor.
data DescribeSpace = DescribeSpace'
  { -- | The ID of the associated Domain.
    domainId :: Prelude.Text,
    -- | The name of the space.
    spaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'describeSpace_domainId' - The ID of the associated Domain.
--
-- 'spaceName', 'describeSpace_spaceName' - The name of the space.
newDescribeSpace ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'spaceName'
  Prelude.Text ->
  DescribeSpace
newDescribeSpace pDomainId_ pSpaceName_ =
  DescribeSpace'
    { domainId = pDomainId_,
      spaceName = pSpaceName_
    }

-- | The ID of the associated Domain.
describeSpace_domainId :: Lens.Lens' DescribeSpace Prelude.Text
describeSpace_domainId = Lens.lens (\DescribeSpace' {domainId} -> domainId) (\s@DescribeSpace' {} a -> s {domainId = a} :: DescribeSpace)

-- | The name of the space.
describeSpace_spaceName :: Lens.Lens' DescribeSpace Prelude.Text
describeSpace_spaceName = Lens.lens (\DescribeSpace' {spaceName} -> spaceName) (\s@DescribeSpace' {} a -> s {spaceName = a} :: DescribeSpace)

instance Core.AWSRequest DescribeSpace where
  type
    AWSResponse DescribeSpace =
      DescribeSpaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSpaceResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DomainId")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "HomeEfsFileSystemUid")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "SpaceArn")
            Prelude.<*> (x Data..?> "SpaceName")
            Prelude.<*> (x Data..?> "SpaceSettings")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSpace where
  hashWithSalt _salt DescribeSpace' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` spaceName

instance Prelude.NFData DescribeSpace where
  rnf DescribeSpace' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf spaceName

instance Data.ToHeaders DescribeSpace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeSpace" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSpace where
  toJSON DescribeSpace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("SpaceName" Data..= spaceName)
          ]
      )

instance Data.ToPath DescribeSpace where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSpace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSpaceResponse' smart constructor.
data DescribeSpaceResponse = DescribeSpaceResponse'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the associated Domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the space\'s profile in the Amazon Elastic File System volume.
    homeEfsFileSystemUid :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The space\'s Amazon Resource Name (ARN).
    spaceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the space.
    spaceName :: Prelude.Maybe Prelude.Text,
    -- | A collection of space settings.
    spaceSettings :: Prelude.Maybe SpaceSettings,
    -- | The status.
    status :: Prelude.Maybe SpaceStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeSpaceResponse_creationTime' - The creation time.
--
-- 'domainId', 'describeSpaceResponse_domainId' - The ID of the associated Domain.
--
-- 'failureReason', 'describeSpaceResponse_failureReason' - The failure reason.
--
-- 'homeEfsFileSystemUid', 'describeSpaceResponse_homeEfsFileSystemUid' - The ID of the space\'s profile in the Amazon Elastic File System volume.
--
-- 'lastModifiedTime', 'describeSpaceResponse_lastModifiedTime' - The last modified time.
--
-- 'spaceArn', 'describeSpaceResponse_spaceArn' - The space\'s Amazon Resource Name (ARN).
--
-- 'spaceName', 'describeSpaceResponse_spaceName' - The name of the space.
--
-- 'spaceSettings', 'describeSpaceResponse_spaceSettings' - A collection of space settings.
--
-- 'status', 'describeSpaceResponse_status' - The status.
--
-- 'httpStatus', 'describeSpaceResponse_httpStatus' - The response's http status code.
newDescribeSpaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSpaceResponse
newDescribeSpaceResponse pHttpStatus_ =
  DescribeSpaceResponse'
    { creationTime =
        Prelude.Nothing,
      domainId = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      homeEfsFileSystemUid = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      spaceArn = Prelude.Nothing,
      spaceName = Prelude.Nothing,
      spaceSettings = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation time.
describeSpaceResponse_creationTime :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.UTCTime)
describeSpaceResponse_creationTime = Lens.lens (\DescribeSpaceResponse' {creationTime} -> creationTime) (\s@DescribeSpaceResponse' {} a -> s {creationTime = a} :: DescribeSpaceResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the associated Domain.
describeSpaceResponse_domainId :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.Text)
describeSpaceResponse_domainId = Lens.lens (\DescribeSpaceResponse' {domainId} -> domainId) (\s@DescribeSpaceResponse' {} a -> s {domainId = a} :: DescribeSpaceResponse)

-- | The failure reason.
describeSpaceResponse_failureReason :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.Text)
describeSpaceResponse_failureReason = Lens.lens (\DescribeSpaceResponse' {failureReason} -> failureReason) (\s@DescribeSpaceResponse' {} a -> s {failureReason = a} :: DescribeSpaceResponse)

-- | The ID of the space\'s profile in the Amazon Elastic File System volume.
describeSpaceResponse_homeEfsFileSystemUid :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.Text)
describeSpaceResponse_homeEfsFileSystemUid = Lens.lens (\DescribeSpaceResponse' {homeEfsFileSystemUid} -> homeEfsFileSystemUid) (\s@DescribeSpaceResponse' {} a -> s {homeEfsFileSystemUid = a} :: DescribeSpaceResponse)

-- | The last modified time.
describeSpaceResponse_lastModifiedTime :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.UTCTime)
describeSpaceResponse_lastModifiedTime = Lens.lens (\DescribeSpaceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeSpaceResponse' {} a -> s {lastModifiedTime = a} :: DescribeSpaceResponse) Prelude.. Lens.mapping Data._Time

-- | The space\'s Amazon Resource Name (ARN).
describeSpaceResponse_spaceArn :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.Text)
describeSpaceResponse_spaceArn = Lens.lens (\DescribeSpaceResponse' {spaceArn} -> spaceArn) (\s@DescribeSpaceResponse' {} a -> s {spaceArn = a} :: DescribeSpaceResponse)

-- | The name of the space.
describeSpaceResponse_spaceName :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe Prelude.Text)
describeSpaceResponse_spaceName = Lens.lens (\DescribeSpaceResponse' {spaceName} -> spaceName) (\s@DescribeSpaceResponse' {} a -> s {spaceName = a} :: DescribeSpaceResponse)

-- | A collection of space settings.
describeSpaceResponse_spaceSettings :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe SpaceSettings)
describeSpaceResponse_spaceSettings = Lens.lens (\DescribeSpaceResponse' {spaceSettings} -> spaceSettings) (\s@DescribeSpaceResponse' {} a -> s {spaceSettings = a} :: DescribeSpaceResponse)

-- | The status.
describeSpaceResponse_status :: Lens.Lens' DescribeSpaceResponse (Prelude.Maybe SpaceStatus)
describeSpaceResponse_status = Lens.lens (\DescribeSpaceResponse' {status} -> status) (\s@DescribeSpaceResponse' {} a -> s {status = a} :: DescribeSpaceResponse)

-- | The response's http status code.
describeSpaceResponse_httpStatus :: Lens.Lens' DescribeSpaceResponse Prelude.Int
describeSpaceResponse_httpStatus = Lens.lens (\DescribeSpaceResponse' {httpStatus} -> httpStatus) (\s@DescribeSpaceResponse' {} a -> s {httpStatus = a} :: DescribeSpaceResponse)

instance Prelude.NFData DescribeSpaceResponse where
  rnf DescribeSpaceResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf homeEfsFileSystemUid
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf spaceArn
      `Prelude.seq` Prelude.rnf spaceName
      `Prelude.seq` Prelude.rnf spaceSettings
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
