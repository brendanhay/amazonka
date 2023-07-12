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
-- Module      : Amazonka.Redshift.DeleteUsageLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage limit from a cluster.
module Amazonka.Redshift.DeleteUsageLimit
  ( -- * Creating a Request
    DeleteUsageLimit (..),
    newDeleteUsageLimit,

    -- * Request Lenses
    deleteUsageLimit_usageLimitId,

    -- * Destructuring the Response
    DeleteUsageLimitResponse (..),
    newDeleteUsageLimitResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUsageLimit' smart constructor.
data DeleteUsageLimit = DeleteUsageLimit'
  { -- | The identifier of the usage limit to delete.
    usageLimitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimitId', 'deleteUsageLimit_usageLimitId' - The identifier of the usage limit to delete.
newDeleteUsageLimit ::
  -- | 'usageLimitId'
  Prelude.Text ->
  DeleteUsageLimit
newDeleteUsageLimit pUsageLimitId_ =
  DeleteUsageLimit' {usageLimitId = pUsageLimitId_}

-- | The identifier of the usage limit to delete.
deleteUsageLimit_usageLimitId :: Lens.Lens' DeleteUsageLimit Prelude.Text
deleteUsageLimit_usageLimitId = Lens.lens (\DeleteUsageLimit' {usageLimitId} -> usageLimitId) (\s@DeleteUsageLimit' {} a -> s {usageLimitId = a} :: DeleteUsageLimit)

instance Core.AWSRequest DeleteUsageLimit where
  type
    AWSResponse DeleteUsageLimit =
      DeleteUsageLimitResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteUsageLimitResponse'

instance Prelude.Hashable DeleteUsageLimit where
  hashWithSalt _salt DeleteUsageLimit' {..} =
    _salt `Prelude.hashWithSalt` usageLimitId

instance Prelude.NFData DeleteUsageLimit where
  rnf DeleteUsageLimit' {..} = Prelude.rnf usageLimitId

instance Data.ToHeaders DeleteUsageLimit where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteUsageLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUsageLimit where
  toQuery DeleteUsageLimit' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteUsageLimit" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "UsageLimitId" Data.=: usageLimitId
      ]

-- | /See:/ 'newDeleteUsageLimitResponse' smart constructor.
data DeleteUsageLimitResponse = DeleteUsageLimitResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsageLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsageLimitResponse ::
  DeleteUsageLimitResponse
newDeleteUsageLimitResponse =
  DeleteUsageLimitResponse'

instance Prelude.NFData DeleteUsageLimitResponse where
  rnf _ = ()
