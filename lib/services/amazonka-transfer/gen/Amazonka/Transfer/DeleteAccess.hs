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
-- Module      : Amazonka.Transfer.DeleteAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to delete the access specified in the @ServerID@ and
-- @ExternalID@ parameters.
module Amazonka.Transfer.DeleteAccess
  ( -- * Creating a Request
    DeleteAccess (..),
    newDeleteAccess,

    -- * Request Lenses
    deleteAccess_serverId,
    deleteAccess_externalId,

    -- * Destructuring the Response
    DeleteAccessResponse (..),
    newDeleteAccessResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDeleteAccess' smart constructor.
data DeleteAccess = DeleteAccess'
  { -- | A system-assigned unique identifier for a server that has this user
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
-- Create a value of 'DeleteAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'deleteAccess_serverId' - A system-assigned unique identifier for a server that has this user
-- assigned.
--
-- 'externalId', 'deleteAccess_externalId' - A unique identifier that is required to identify specific groups within
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
newDeleteAccess ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  DeleteAccess
newDeleteAccess pServerId_ pExternalId_ =
  DeleteAccess'
    { serverId = pServerId_,
      externalId = pExternalId_
    }

-- | A system-assigned unique identifier for a server that has this user
-- assigned.
deleteAccess_serverId :: Lens.Lens' DeleteAccess Prelude.Text
deleteAccess_serverId = Lens.lens (\DeleteAccess' {serverId} -> serverId) (\s@DeleteAccess' {} a -> s {serverId = a} :: DeleteAccess)

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
deleteAccess_externalId :: Lens.Lens' DeleteAccess Prelude.Text
deleteAccess_externalId = Lens.lens (\DeleteAccess' {externalId} -> externalId) (\s@DeleteAccess' {} a -> s {externalId = a} :: DeleteAccess)

instance Core.AWSRequest DeleteAccess where
  type AWSResponse DeleteAccess = DeleteAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteAccessResponse'

instance Prelude.Hashable DeleteAccess where
  hashWithSalt _salt DeleteAccess' {..} =
    _salt
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` externalId

instance Prelude.NFData DeleteAccess where
  rnf DeleteAccess' {..} =
    Prelude.rnf serverId `Prelude.seq`
      Prelude.rnf externalId

instance Data.ToHeaders DeleteAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DeleteAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccess where
  toJSON DeleteAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("ExternalId" Data..= externalId)
          ]
      )

instance Data.ToPath DeleteAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessResponse' smart constructor.
data DeleteAccessResponse = DeleteAccessResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccessResponse ::
  DeleteAccessResponse
newDeleteAccessResponse = DeleteAccessResponse'

instance Prelude.NFData DeleteAccessResponse where
  rnf _ = ()
