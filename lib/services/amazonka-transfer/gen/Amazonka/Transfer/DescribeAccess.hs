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
-- Module      : Amazonka.Transfer.DescribeAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the access that is assigned to the specific file transfer
-- protocol-enabled server, as identified by its @ServerId@ property and
-- its @ExternalId@.
--
-- The response from this call returns the properties of the access that is
-- associated with the @ServerId@ value that was specified.
module Amazonka.Transfer.DescribeAccess
  ( -- * Creating a Request
    DescribeAccess (..),
    newDescribeAccess,

    -- * Request Lenses
    describeAccess_serverId,
    describeAccess_externalId,

    -- * Destructuring the Response
    DescribeAccessResponse (..),
    newDescribeAccessResponse,

    -- * Response Lenses
    describeAccessResponse_httpStatus,
    describeAccessResponse_serverId,
    describeAccessResponse_access,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeAccess' smart constructor.
data DescribeAccess = DescribeAccess'
  { -- | A system-assigned unique identifier for a server that has this access
    -- assigned.
    serverId :: Prelude.Text,
    -- | A unique identifier that is required to identify specific groups within
    -- your directory. The users of the group that you associate have access to
    -- your Amazon S3 or Amazon EFS resources over the enabled protocols using
    -- Transfer Family. If you know the group name, you can view the SID values
    -- by running the following command using Windows PowerShell.
    --
    -- @Get-ADGroup -Filter {samAccountName -like \"@/@YourGroupName@/@*\"} -Properties * | Select SamAccountName,ObjectSid@
    --
    -- In that command, replace /YourGroupName/ with the name of your Active
    -- Directory group.
    --
    -- The regular expression used to validate this parameter is a string of
    -- characters consisting of uppercase and lowercase alphanumeric characters
    -- with no spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'describeAccess_serverId' - A system-assigned unique identifier for a server that has this access
-- assigned.
--
-- 'externalId', 'describeAccess_externalId' - A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family. If you know the group name, you can view the SID values
-- by running the following command using Windows PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"@/@YourGroupName@/@*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regular expression used to validate this parameter is a string of
-- characters consisting of uppercase and lowercase alphanumeric characters
-- with no spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
newDescribeAccess ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  DescribeAccess
newDescribeAccess pServerId_ pExternalId_ =
  DescribeAccess'
    { serverId = pServerId_,
      externalId = pExternalId_
    }

-- | A system-assigned unique identifier for a server that has this access
-- assigned.
describeAccess_serverId :: Lens.Lens' DescribeAccess Prelude.Text
describeAccess_serverId = Lens.lens (\DescribeAccess' {serverId} -> serverId) (\s@DescribeAccess' {} a -> s {serverId = a} :: DescribeAccess)

-- | A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family. If you know the group name, you can view the SID values
-- by running the following command using Windows PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"@/@YourGroupName@/@*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regular expression used to validate this parameter is a string of
-- characters consisting of uppercase and lowercase alphanumeric characters
-- with no spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
describeAccess_externalId :: Lens.Lens' DescribeAccess Prelude.Text
describeAccess_externalId = Lens.lens (\DescribeAccess' {externalId} -> externalId) (\s@DescribeAccess' {} a -> s {externalId = a} :: DescribeAccess)

instance Core.AWSRequest DescribeAccess where
  type
    AWSResponse DescribeAccess =
      DescribeAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..:> "Access")
      )

instance Prelude.Hashable DescribeAccess where
  hashWithSalt _salt DescribeAccess' {..} =
    _salt
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` externalId

instance Prelude.NFData DescribeAccess where
  rnf DescribeAccess' {..} =
    Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf externalId

instance Data.ToHeaders DescribeAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DescribeAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccess where
  toJSON DescribeAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("ExternalId" Data..= externalId)
          ]
      )

instance Data.ToPath DescribeAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccessResponse' smart constructor.
data DescribeAccessResponse = DescribeAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server that has this access
    -- assigned.
    serverId :: Prelude.Text,
    -- | The external identifier of the server that the access is attached to.
    access :: DescribedAccess
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAccessResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'describeAccessResponse_serverId' - A system-assigned unique identifier for a server that has this access
-- assigned.
--
-- 'access', 'describeAccessResponse_access' - The external identifier of the server that the access is attached to.
newDescribeAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'access'
  DescribedAccess ->
  DescribeAccessResponse
newDescribeAccessResponse
  pHttpStatus_
  pServerId_
  pAccess_ =
    DescribeAccessResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        access = pAccess_
      }

-- | The response's http status code.
describeAccessResponse_httpStatus :: Lens.Lens' DescribeAccessResponse Prelude.Int
describeAccessResponse_httpStatus = Lens.lens (\DescribeAccessResponse' {httpStatus} -> httpStatus) (\s@DescribeAccessResponse' {} a -> s {httpStatus = a} :: DescribeAccessResponse)

-- | A system-assigned unique identifier for a server that has this access
-- assigned.
describeAccessResponse_serverId :: Lens.Lens' DescribeAccessResponse Prelude.Text
describeAccessResponse_serverId = Lens.lens (\DescribeAccessResponse' {serverId} -> serverId) (\s@DescribeAccessResponse' {} a -> s {serverId = a} :: DescribeAccessResponse)

-- | The external identifier of the server that the access is attached to.
describeAccessResponse_access :: Lens.Lens' DescribeAccessResponse DescribedAccess
describeAccessResponse_access = Lens.lens (\DescribeAccessResponse' {access} -> access) (\s@DescribeAccessResponse' {} a -> s {access = a} :: DescribeAccessResponse)

instance Prelude.NFData DescribeAccessResponse where
  rnf DescribeAccessResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf access
