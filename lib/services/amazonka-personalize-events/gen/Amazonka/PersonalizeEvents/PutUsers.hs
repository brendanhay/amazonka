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
-- Module      : Amazonka.PersonalizeEvents.PutUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more users to a Users dataset. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/importing-users.html Importing Users Incrementally>.
module Amazonka.PersonalizeEvents.PutUsers
  ( -- * Creating a Request
    PutUsers (..),
    newPutUsers,

    -- * Request Lenses
    putUsers_datasetArn,
    putUsers_users,

    -- * Destructuring the Response
    PutUsersResponse (..),
    newPutUsersResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PersonalizeEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutUsers' smart constructor.
data PutUsers = PutUsers'
  { -- | The Amazon Resource Name (ARN) of the Users dataset you are adding the
    -- user or users to.
    datasetArn :: Prelude.Text,
    -- | A list of user data.
    users :: Prelude.NonEmpty User
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'putUsers_datasetArn' - The Amazon Resource Name (ARN) of the Users dataset you are adding the
-- user or users to.
--
-- 'users', 'putUsers_users' - A list of user data.
newPutUsers ::
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'users'
  Prelude.NonEmpty User ->
  PutUsers
newPutUsers pDatasetArn_ pUsers_ =
  PutUsers'
    { datasetArn = pDatasetArn_,
      users = Lens.coerced Lens.# pUsers_
    }

-- | The Amazon Resource Name (ARN) of the Users dataset you are adding the
-- user or users to.
putUsers_datasetArn :: Lens.Lens' PutUsers Prelude.Text
putUsers_datasetArn = Lens.lens (\PutUsers' {datasetArn} -> datasetArn) (\s@PutUsers' {} a -> s {datasetArn = a} :: PutUsers)

-- | A list of user data.
putUsers_users :: Lens.Lens' PutUsers (Prelude.NonEmpty User)
putUsers_users = Lens.lens (\PutUsers' {users} -> users) (\s@PutUsers' {} a -> s {users = a} :: PutUsers) Prelude.. Lens.coerced

instance Core.AWSRequest PutUsers where
  type AWSResponse PutUsers = PutUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull PutUsersResponse'

instance Prelude.Hashable PutUsers where
  hashWithSalt _salt PutUsers' {..} =
    _salt
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` users

instance Prelude.NFData PutUsers where
  rnf PutUsers' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf users

instance Data.ToHeaders PutUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutUsers where
  toJSON PutUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("datasetArn" Data..= datasetArn),
            Prelude.Just ("users" Data..= users)
          ]
      )

instance Data.ToPath PutUsers where
  toPath = Prelude.const "/users"

instance Data.ToQuery PutUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutUsersResponse' smart constructor.
data PutUsersResponse = PutUsersResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutUsersResponse ::
  PutUsersResponse
newPutUsersResponse = PutUsersResponse'

instance Prelude.NFData PutUsersResponse where
  rnf _ = ()
