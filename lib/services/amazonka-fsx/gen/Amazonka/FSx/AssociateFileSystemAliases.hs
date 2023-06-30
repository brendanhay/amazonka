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
-- Module      : Amazonka.FSx.AssociateFileSystemAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this action to associate one or more Domain Name Server (DNS)
-- aliases with an existing Amazon FSx for Windows File Server file system.
-- A file system can have a maximum of 50 DNS aliases associated with it at
-- any one time. If you try to associate a DNS alias that is already
-- associated with the file system, FSx takes no action on that alias in
-- the request. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-dns-aliases.html Working with DNS Aliases>
-- and
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/walkthrough05-file-system-custom-CNAME.html Walkthrough 5: Using DNS aliases to access your file system>,
-- including additional steps you must take to be able to access your file
-- system using a DNS alias.
--
-- The system response shows the DNS aliases that Amazon FSx is attempting
-- to associate with the file system. Use the API operation to monitor the
-- status of the aliases Amazon FSx is associating with the file system.
module Amazonka.FSx.AssociateFileSystemAliases
  ( -- * Creating a Request
    AssociateFileSystemAliases (..),
    newAssociateFileSystemAliases,

    -- * Request Lenses
    associateFileSystemAliases_clientRequestToken,
    associateFileSystemAliases_fileSystemId,
    associateFileSystemAliases_aliases,

    -- * Destructuring the Response
    AssociateFileSystemAliasesResponse (..),
    newAssociateFileSystemAliasesResponse,

    -- * Response Lenses
    associateFileSystemAliasesResponse_aliases,
    associateFileSystemAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object specifying one or more DNS alias names to associate
-- with an Amazon FSx for Windows File Server file system.
--
-- /See:/ 'newAssociateFileSystemAliases' smart constructor.
data AssociateFileSystemAliases = AssociateFileSystemAliases'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file system with which you want to associate one or more
    -- DNS aliases.
    fileSystemId :: Prelude.Text,
    -- | An array of one or more DNS alias names to associate with the file
    -- system. The alias name has to comply with the following formatting
    -- requirements:
    --
    -- -   Formatted as a fully-qualified domain name (FQDN),
    --     /@hostname.domain@/ , for example, @accounting.corp.example.com@.
    --
    -- -   Can contain alphanumeric characters and the hyphen (-).
    --
    -- -   Cannot start or end with a hyphen.
    --
    -- -   Can start with a numeric.
    --
    -- For DNS alias names, Amazon FSx stores alphabetic characters as
    -- lowercase letters (a-z), regardless of how you specify them: as
    -- uppercase letters, lowercase letters, or the corresponding letters in
    -- escape codes.
    aliases :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFileSystemAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateFileSystemAliases_clientRequestToken' - Undocumented member.
--
-- 'fileSystemId', 'associateFileSystemAliases_fileSystemId' - Specifies the file system with which you want to associate one or more
-- DNS aliases.
--
-- 'aliases', 'associateFileSystemAliases_aliases' - An array of one or more DNS alias names to associate with the file
-- system. The alias name has to comply with the following formatting
-- requirements:
--
-- -   Formatted as a fully-qualified domain name (FQDN),
--     /@hostname.domain@/ , for example, @accounting.corp.example.com@.
--
-- -   Can contain alphanumeric characters and the hyphen (-).
--
-- -   Cannot start or end with a hyphen.
--
-- -   Can start with a numeric.
--
-- For DNS alias names, Amazon FSx stores alphabetic characters as
-- lowercase letters (a-z), regardless of how you specify them: as
-- uppercase letters, lowercase letters, or the corresponding letters in
-- escape codes.
newAssociateFileSystemAliases ::
  -- | 'fileSystemId'
  Prelude.Text ->
  AssociateFileSystemAliases
newAssociateFileSystemAliases pFileSystemId_ =
  AssociateFileSystemAliases'
    { clientRequestToken =
        Prelude.Nothing,
      fileSystemId = pFileSystemId_,
      aliases = Prelude.mempty
    }

-- | Undocumented member.
associateFileSystemAliases_clientRequestToken :: Lens.Lens' AssociateFileSystemAliases (Prelude.Maybe Prelude.Text)
associateFileSystemAliases_clientRequestToken = Lens.lens (\AssociateFileSystemAliases' {clientRequestToken} -> clientRequestToken) (\s@AssociateFileSystemAliases' {} a -> s {clientRequestToken = a} :: AssociateFileSystemAliases)

-- | Specifies the file system with which you want to associate one or more
-- DNS aliases.
associateFileSystemAliases_fileSystemId :: Lens.Lens' AssociateFileSystemAliases Prelude.Text
associateFileSystemAliases_fileSystemId = Lens.lens (\AssociateFileSystemAliases' {fileSystemId} -> fileSystemId) (\s@AssociateFileSystemAliases' {} a -> s {fileSystemId = a} :: AssociateFileSystemAliases)

-- | An array of one or more DNS alias names to associate with the file
-- system. The alias name has to comply with the following formatting
-- requirements:
--
-- -   Formatted as a fully-qualified domain name (FQDN),
--     /@hostname.domain@/ , for example, @accounting.corp.example.com@.
--
-- -   Can contain alphanumeric characters and the hyphen (-).
--
-- -   Cannot start or end with a hyphen.
--
-- -   Can start with a numeric.
--
-- For DNS alias names, Amazon FSx stores alphabetic characters as
-- lowercase letters (a-z), regardless of how you specify them: as
-- uppercase letters, lowercase letters, or the corresponding letters in
-- escape codes.
associateFileSystemAliases_aliases :: Lens.Lens' AssociateFileSystemAliases [Prelude.Text]
associateFileSystemAliases_aliases = Lens.lens (\AssociateFileSystemAliases' {aliases} -> aliases) (\s@AssociateFileSystemAliases' {} a -> s {aliases = a} :: AssociateFileSystemAliases) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateFileSystemAliases where
  type
    AWSResponse AssociateFileSystemAliases =
      AssociateFileSystemAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateFileSystemAliasesResponse'
            Prelude.<$> (x Data..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFileSystemAliases where
  hashWithSalt _salt AssociateFileSystemAliases' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` aliases

instance Prelude.NFData AssociateFileSystemAliases where
  rnf AssociateFileSystemAliases' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf aliases

instance Data.ToHeaders AssociateFileSystemAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.AssociateFileSystemAliases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFileSystemAliases where
  toJSON AssociateFileSystemAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just ("Aliases" Data..= aliases)
          ]
      )

instance Data.ToPath AssociateFileSystemAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFileSystemAliases where
  toQuery = Prelude.const Prelude.mempty

-- | The system generated response showing the DNS aliases that Amazon FSx is
-- attempting to associate with the file system. Use the API operation to
-- monitor the status of the aliases Amazon FSx is associating with the
-- file system. It can take up to 2.5 minutes for the alias status to
-- change from @CREATING@ to @AVAILABLE@.
--
-- /See:/ 'newAssociateFileSystemAliasesResponse' smart constructor.
data AssociateFileSystemAliasesResponse = AssociateFileSystemAliasesResponse'
  { -- | An array of the DNS aliases that Amazon FSx is associating with the file
    -- system.
    aliases :: Prelude.Maybe [Alias],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFileSystemAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliases', 'associateFileSystemAliasesResponse_aliases' - An array of the DNS aliases that Amazon FSx is associating with the file
-- system.
--
-- 'httpStatus', 'associateFileSystemAliasesResponse_httpStatus' - The response's http status code.
newAssociateFileSystemAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFileSystemAliasesResponse
newAssociateFileSystemAliasesResponse pHttpStatus_ =
  AssociateFileSystemAliasesResponse'
    { aliases =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of the DNS aliases that Amazon FSx is associating with the file
-- system.
associateFileSystemAliasesResponse_aliases :: Lens.Lens' AssociateFileSystemAliasesResponse (Prelude.Maybe [Alias])
associateFileSystemAliasesResponse_aliases = Lens.lens (\AssociateFileSystemAliasesResponse' {aliases} -> aliases) (\s@AssociateFileSystemAliasesResponse' {} a -> s {aliases = a} :: AssociateFileSystemAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associateFileSystemAliasesResponse_httpStatus :: Lens.Lens' AssociateFileSystemAliasesResponse Prelude.Int
associateFileSystemAliasesResponse_httpStatus = Lens.lens (\AssociateFileSystemAliasesResponse' {httpStatus} -> httpStatus) (\s@AssociateFileSystemAliasesResponse' {} a -> s {httpStatus = a} :: AssociateFileSystemAliasesResponse)

instance
  Prelude.NFData
    AssociateFileSystemAliasesResponse
  where
  rnf AssociateFileSystemAliasesResponse' {..} =
    Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf httpStatus
