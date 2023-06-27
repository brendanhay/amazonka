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
-- Module      : Amazonka.FSx.DisassociateFileSystemAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this action to disassociate, or remove, one or more Domain Name
-- Service (DNS) aliases from an Amazon FSx for Windows File Server file
-- system. If you attempt to disassociate a DNS alias that is not
-- associated with the file system, Amazon FSx responds with a 400 Bad
-- Request. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-dns-aliases.html Working with DNS Aliases>.
--
-- The system generated response showing the DNS aliases that Amazon FSx is
-- attempting to disassociate from the file system. Use the API operation
-- to monitor the status of the aliases Amazon FSx is disassociating with
-- the file system.
module Amazonka.FSx.DisassociateFileSystemAliases
  ( -- * Creating a Request
    DisassociateFileSystemAliases (..),
    newDisassociateFileSystemAliases,

    -- * Request Lenses
    disassociateFileSystemAliases_clientRequestToken,
    disassociateFileSystemAliases_fileSystemId,
    disassociateFileSystemAliases_aliases,

    -- * Destructuring the Response
    DisassociateFileSystemAliasesResponse (..),
    newDisassociateFileSystemAliasesResponse,

    -- * Response Lenses
    disassociateFileSystemAliasesResponse_aliases,
    disassociateFileSystemAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object of DNS aliases to disassociate from an Amazon FSx for
-- Windows File Server file system.
--
-- /See:/ 'newDisassociateFileSystemAliases' smart constructor.
data DisassociateFileSystemAliases = DisassociateFileSystemAliases'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file system from which to disassociate the DNS aliases.
    fileSystemId :: Prelude.Text,
    -- | An array of one or more DNS alias names to disassociate, or remove, from
    -- the file system.
    aliases :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFileSystemAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'disassociateFileSystemAliases_clientRequestToken' - Undocumented member.
--
-- 'fileSystemId', 'disassociateFileSystemAliases_fileSystemId' - Specifies the file system from which to disassociate the DNS aliases.
--
-- 'aliases', 'disassociateFileSystemAliases_aliases' - An array of one or more DNS alias names to disassociate, or remove, from
-- the file system.
newDisassociateFileSystemAliases ::
  -- | 'fileSystemId'
  Prelude.Text ->
  DisassociateFileSystemAliases
newDisassociateFileSystemAliases pFileSystemId_ =
  DisassociateFileSystemAliases'
    { clientRequestToken =
        Prelude.Nothing,
      fileSystemId = pFileSystemId_,
      aliases = Prelude.mempty
    }

-- | Undocumented member.
disassociateFileSystemAliases_clientRequestToken :: Lens.Lens' DisassociateFileSystemAliases (Prelude.Maybe Prelude.Text)
disassociateFileSystemAliases_clientRequestToken = Lens.lens (\DisassociateFileSystemAliases' {clientRequestToken} -> clientRequestToken) (\s@DisassociateFileSystemAliases' {} a -> s {clientRequestToken = a} :: DisassociateFileSystemAliases)

-- | Specifies the file system from which to disassociate the DNS aliases.
disassociateFileSystemAliases_fileSystemId :: Lens.Lens' DisassociateFileSystemAliases Prelude.Text
disassociateFileSystemAliases_fileSystemId = Lens.lens (\DisassociateFileSystemAliases' {fileSystemId} -> fileSystemId) (\s@DisassociateFileSystemAliases' {} a -> s {fileSystemId = a} :: DisassociateFileSystemAliases)

-- | An array of one or more DNS alias names to disassociate, or remove, from
-- the file system.
disassociateFileSystemAliases_aliases :: Lens.Lens' DisassociateFileSystemAliases [Prelude.Text]
disassociateFileSystemAliases_aliases = Lens.lens (\DisassociateFileSystemAliases' {aliases} -> aliases) (\s@DisassociateFileSystemAliases' {} a -> s {aliases = a} :: DisassociateFileSystemAliases) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociateFileSystemAliases
  where
  type
    AWSResponse DisassociateFileSystemAliases =
      DisassociateFileSystemAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateFileSystemAliasesResponse'
            Prelude.<$> (x Data..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateFileSystemAliases
  where
  hashWithSalt _salt DisassociateFileSystemAliases' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` aliases

instance Prelude.NFData DisassociateFileSystemAliases where
  rnf DisassociateFileSystemAliases' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf aliases

instance Data.ToHeaders DisassociateFileSystemAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DisassociateFileSystemAliases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFileSystemAliases where
  toJSON DisassociateFileSystemAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just ("Aliases" Data..= aliases)
          ]
      )

instance Data.ToPath DisassociateFileSystemAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateFileSystemAliases where
  toQuery = Prelude.const Prelude.mempty

-- | The system generated response showing the DNS aliases that Amazon FSx is
-- attempting to disassociate from the file system. Use the API operation
-- to monitor the status of the aliases Amazon FSx is removing from the
-- file system.
--
-- /See:/ 'newDisassociateFileSystemAliasesResponse' smart constructor.
data DisassociateFileSystemAliasesResponse = DisassociateFileSystemAliasesResponse'
  { -- | An array of one or more DNS aliases that Amazon FSx is attempting to
    -- disassociate from the file system.
    aliases :: Prelude.Maybe [Alias],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFileSystemAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliases', 'disassociateFileSystemAliasesResponse_aliases' - An array of one or more DNS aliases that Amazon FSx is attempting to
-- disassociate from the file system.
--
-- 'httpStatus', 'disassociateFileSystemAliasesResponse_httpStatus' - The response's http status code.
newDisassociateFileSystemAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFileSystemAliasesResponse
newDisassociateFileSystemAliasesResponse pHttpStatus_ =
  DisassociateFileSystemAliasesResponse'
    { aliases =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of one or more DNS aliases that Amazon FSx is attempting to
-- disassociate from the file system.
disassociateFileSystemAliasesResponse_aliases :: Lens.Lens' DisassociateFileSystemAliasesResponse (Prelude.Maybe [Alias])
disassociateFileSystemAliasesResponse_aliases = Lens.lens (\DisassociateFileSystemAliasesResponse' {aliases} -> aliases) (\s@DisassociateFileSystemAliasesResponse' {} a -> s {aliases = a} :: DisassociateFileSystemAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociateFileSystemAliasesResponse_httpStatus :: Lens.Lens' DisassociateFileSystemAliasesResponse Prelude.Int
disassociateFileSystemAliasesResponse_httpStatus = Lens.lens (\DisassociateFileSystemAliasesResponse' {httpStatus} -> httpStatus) (\s@DisassociateFileSystemAliasesResponse' {} a -> s {httpStatus = a} :: DisassociateFileSystemAliasesResponse)

instance
  Prelude.NFData
    DisassociateFileSystemAliasesResponse
  where
  rnf DisassociateFileSystemAliasesResponse' {..} =
    Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf httpStatus
