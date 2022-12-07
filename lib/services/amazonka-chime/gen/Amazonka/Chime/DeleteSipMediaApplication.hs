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
-- Module      : Amazonka.Chime.DeleteSipMediaApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SIP media application.
module Amazonka.Chime.DeleteSipMediaApplication
  ( -- * Creating a Request
    DeleteSipMediaApplication (..),
    newDeleteSipMediaApplication,

    -- * Request Lenses
    deleteSipMediaApplication_sipMediaApplicationId,

    -- * Destructuring the Response
    DeleteSipMediaApplicationResponse (..),
    newDeleteSipMediaApplicationResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSipMediaApplication' smart constructor.
data DeleteSipMediaApplication = DeleteSipMediaApplication'
  { -- | The SIP media application ID.
    sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSipMediaApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationId', 'deleteSipMediaApplication_sipMediaApplicationId' - The SIP media application ID.
newDeleteSipMediaApplication ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  DeleteSipMediaApplication
newDeleteSipMediaApplication pSipMediaApplicationId_ =
  DeleteSipMediaApplication'
    { sipMediaApplicationId =
        pSipMediaApplicationId_
    }

-- | The SIP media application ID.
deleteSipMediaApplication_sipMediaApplicationId :: Lens.Lens' DeleteSipMediaApplication Prelude.Text
deleteSipMediaApplication_sipMediaApplicationId = Lens.lens (\DeleteSipMediaApplication' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@DeleteSipMediaApplication' {} a -> s {sipMediaApplicationId = a} :: DeleteSipMediaApplication)

instance Core.AWSRequest DeleteSipMediaApplication where
  type
    AWSResponse DeleteSipMediaApplication =
      DeleteSipMediaApplicationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSipMediaApplicationResponse'

instance Prelude.Hashable DeleteSipMediaApplication where
  hashWithSalt _salt DeleteSipMediaApplication' {..} =
    _salt `Prelude.hashWithSalt` sipMediaApplicationId

instance Prelude.NFData DeleteSipMediaApplication where
  rnf DeleteSipMediaApplication' {..} =
    Prelude.rnf sipMediaApplicationId

instance Data.ToHeaders DeleteSipMediaApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSipMediaApplication where
  toPath DeleteSipMediaApplication' {..} =
    Prelude.mconcat
      [ "/sip-media-applications/",
        Data.toBS sipMediaApplicationId
      ]

instance Data.ToQuery DeleteSipMediaApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSipMediaApplicationResponse' smart constructor.
data DeleteSipMediaApplicationResponse = DeleteSipMediaApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSipMediaApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSipMediaApplicationResponse ::
  DeleteSipMediaApplicationResponse
newDeleteSipMediaApplicationResponse =
  DeleteSipMediaApplicationResponse'

instance
  Prelude.NFData
    DeleteSipMediaApplicationResponse
  where
  rnf _ = ()
