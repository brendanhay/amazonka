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
-- Module      : Amazonka.WellArchitected.DeleteProfileShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a profile share.
module Amazonka.WellArchitected.DeleteProfileShare
  ( -- * Creating a Request
    DeleteProfileShare (..),
    newDeleteProfileShare,

    -- * Request Lenses
    deleteProfileShare_shareId,
    deleteProfileShare_profileArn,
    deleteProfileShare_clientRequestToken,

    -- * Destructuring the Response
    DeleteProfileShareResponse (..),
    newDeleteProfileShareResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newDeleteProfileShare' smart constructor.
data DeleteProfileShare = DeleteProfileShare'
  { shareId :: Prelude.Text,
    -- | The profile ARN.
    profileArn :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'deleteProfileShare_shareId' - Undocumented member.
--
-- 'profileArn', 'deleteProfileShare_profileArn' - The profile ARN.
--
-- 'clientRequestToken', 'deleteProfileShare_clientRequestToken' - Undocumented member.
newDeleteProfileShare ::
  -- | 'shareId'
  Prelude.Text ->
  -- | 'profileArn'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  DeleteProfileShare
newDeleteProfileShare
  pShareId_
  pProfileArn_
  pClientRequestToken_ =
    DeleteProfileShare'
      { shareId = pShareId_,
        profileArn = pProfileArn_,
        clientRequestToken = pClientRequestToken_
      }

-- | Undocumented member.
deleteProfileShare_shareId :: Lens.Lens' DeleteProfileShare Prelude.Text
deleteProfileShare_shareId = Lens.lens (\DeleteProfileShare' {shareId} -> shareId) (\s@DeleteProfileShare' {} a -> s {shareId = a} :: DeleteProfileShare)

-- | The profile ARN.
deleteProfileShare_profileArn :: Lens.Lens' DeleteProfileShare Prelude.Text
deleteProfileShare_profileArn = Lens.lens (\DeleteProfileShare' {profileArn} -> profileArn) (\s@DeleteProfileShare' {} a -> s {profileArn = a} :: DeleteProfileShare)

-- | Undocumented member.
deleteProfileShare_clientRequestToken :: Lens.Lens' DeleteProfileShare Prelude.Text
deleteProfileShare_clientRequestToken = Lens.lens (\DeleteProfileShare' {clientRequestToken} -> clientRequestToken) (\s@DeleteProfileShare' {} a -> s {clientRequestToken = a} :: DeleteProfileShare)

instance Core.AWSRequest DeleteProfileShare where
  type
    AWSResponse DeleteProfileShare =
      DeleteProfileShareResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteProfileShareResponse'

instance Prelude.Hashable DeleteProfileShare where
  hashWithSalt _salt DeleteProfileShare' {..} =
    _salt
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData DeleteProfileShare where
  rnf DeleteProfileShare' {..} =
    Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders DeleteProfileShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteProfileShare where
  toPath DeleteProfileShare' {..} =
    Prelude.mconcat
      [ "/profiles/",
        Data.toBS profileArn,
        "/shares/",
        Data.toBS shareId
      ]

instance Data.ToQuery DeleteProfileShare where
  toQuery DeleteProfileShare' {..} =
    Prelude.mconcat
      ["ClientRequestToken" Data.=: clientRequestToken]

-- | /See:/ 'newDeleteProfileShareResponse' smart constructor.
data DeleteProfileShareResponse = DeleteProfileShareResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProfileShareResponse ::
  DeleteProfileShareResponse
newDeleteProfileShareResponse =
  DeleteProfileShareResponse'

instance Prelude.NFData DeleteProfileShareResponse where
  rnf _ = ()
