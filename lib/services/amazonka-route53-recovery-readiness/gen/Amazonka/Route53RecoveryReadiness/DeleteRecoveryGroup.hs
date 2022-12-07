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
-- Module      : Amazonka.Route53RecoveryReadiness.DeleteRecoveryGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a recovery group.
module Amazonka.Route53RecoveryReadiness.DeleteRecoveryGroup
  ( -- * Creating a Request
    DeleteRecoveryGroup (..),
    newDeleteRecoveryGroup,

    -- * Request Lenses
    deleteRecoveryGroup_recoveryGroupName,

    -- * Destructuring the Response
    DeleteRecoveryGroupResponse (..),
    newDeleteRecoveryGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteRecoveryGroup' smart constructor.
data DeleteRecoveryGroup = DeleteRecoveryGroup'
  { -- | The name of a recovery group.
    recoveryGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecoveryGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryGroupName', 'deleteRecoveryGroup_recoveryGroupName' - The name of a recovery group.
newDeleteRecoveryGroup ::
  -- | 'recoveryGroupName'
  Prelude.Text ->
  DeleteRecoveryGroup
newDeleteRecoveryGroup pRecoveryGroupName_ =
  DeleteRecoveryGroup'
    { recoveryGroupName =
        pRecoveryGroupName_
    }

-- | The name of a recovery group.
deleteRecoveryGroup_recoveryGroupName :: Lens.Lens' DeleteRecoveryGroup Prelude.Text
deleteRecoveryGroup_recoveryGroupName = Lens.lens (\DeleteRecoveryGroup' {recoveryGroupName} -> recoveryGroupName) (\s@DeleteRecoveryGroup' {} a -> s {recoveryGroupName = a} :: DeleteRecoveryGroup)

instance Core.AWSRequest DeleteRecoveryGroup where
  type
    AWSResponse DeleteRecoveryGroup =
      DeleteRecoveryGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRecoveryGroupResponse'

instance Prelude.Hashable DeleteRecoveryGroup where
  hashWithSalt _salt DeleteRecoveryGroup' {..} =
    _salt `Prelude.hashWithSalt` recoveryGroupName

instance Prelude.NFData DeleteRecoveryGroup where
  rnf DeleteRecoveryGroup' {..} =
    Prelude.rnf recoveryGroupName

instance Data.ToHeaders DeleteRecoveryGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRecoveryGroup where
  toPath DeleteRecoveryGroup' {..} =
    Prelude.mconcat
      ["/recoverygroups/", Data.toBS recoveryGroupName]

instance Data.ToQuery DeleteRecoveryGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecoveryGroupResponse' smart constructor.
data DeleteRecoveryGroupResponse = DeleteRecoveryGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecoveryGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRecoveryGroupResponse ::
  DeleteRecoveryGroupResponse
newDeleteRecoveryGroupResponse =
  DeleteRecoveryGroupResponse'

instance Prelude.NFData DeleteRecoveryGroupResponse where
  rnf _ = ()
