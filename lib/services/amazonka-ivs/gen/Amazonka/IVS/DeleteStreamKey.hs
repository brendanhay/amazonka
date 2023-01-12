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
-- Module      : Amazonka.IVS.DeleteStreamKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream key for the specified ARN, so it can no longer be
-- used to stream.
module Amazonka.IVS.DeleteStreamKey
  ( -- * Creating a Request
    DeleteStreamKey (..),
    newDeleteStreamKey,

    -- * Request Lenses
    deleteStreamKey_arn,

    -- * Destructuring the Response
    DeleteStreamKeyResponse (..),
    newDeleteStreamKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStreamKey' smart constructor.
data DeleteStreamKey = DeleteStreamKey'
  { -- | ARN of the stream key to be deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteStreamKey_arn' - ARN of the stream key to be deleted.
newDeleteStreamKey ::
  -- | 'arn'
  Prelude.Text ->
  DeleteStreamKey
newDeleteStreamKey pArn_ =
  DeleteStreamKey' {arn = pArn_}

-- | ARN of the stream key to be deleted.
deleteStreamKey_arn :: Lens.Lens' DeleteStreamKey Prelude.Text
deleteStreamKey_arn = Lens.lens (\DeleteStreamKey' {arn} -> arn) (\s@DeleteStreamKey' {} a -> s {arn = a} :: DeleteStreamKey)

instance Core.AWSRequest DeleteStreamKey where
  type
    AWSResponse DeleteStreamKey =
      DeleteStreamKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteStreamKeyResponse'

instance Prelude.Hashable DeleteStreamKey where
  hashWithSalt _salt DeleteStreamKey' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteStreamKey where
  rnf DeleteStreamKey' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteStreamKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStreamKey where
  toJSON DeleteStreamKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteStreamKey where
  toPath = Prelude.const "/DeleteStreamKey"

instance Data.ToQuery DeleteStreamKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamKeyResponse' smart constructor.
data DeleteStreamKeyResponse = DeleteStreamKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStreamKeyResponse ::
  DeleteStreamKeyResponse
newDeleteStreamKeyResponse = DeleteStreamKeyResponse'

instance Prelude.NFData DeleteStreamKeyResponse where
  rnf _ = ()
