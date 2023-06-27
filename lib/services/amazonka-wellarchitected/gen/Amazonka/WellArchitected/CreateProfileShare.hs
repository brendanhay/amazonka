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
-- Module      : Amazonka.WellArchitected.CreateProfileShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a profile share.
module Amazonka.WellArchitected.CreateProfileShare
  ( -- * Creating a Request
    CreateProfileShare (..),
    newCreateProfileShare,

    -- * Request Lenses
    createProfileShare_profileArn,
    createProfileShare_sharedWith,
    createProfileShare_clientRequestToken,

    -- * Destructuring the Response
    CreateProfileShareResponse (..),
    newCreateProfileShareResponse,

    -- * Response Lenses
    createProfileShareResponse_profileArn,
    createProfileShareResponse_shareId,
    createProfileShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newCreateProfileShare' smart constructor.
data CreateProfileShare = CreateProfileShare'
  { -- | The profile ARN.
    profileArn :: Prelude.Text,
    sharedWith :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'createProfileShare_profileArn' - The profile ARN.
--
-- 'sharedWith', 'createProfileShare_sharedWith' - Undocumented member.
--
-- 'clientRequestToken', 'createProfileShare_clientRequestToken' - Undocumented member.
newCreateProfileShare ::
  -- | 'profileArn'
  Prelude.Text ->
  -- | 'sharedWith'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateProfileShare
newCreateProfileShare
  pProfileArn_
  pSharedWith_
  pClientRequestToken_ =
    CreateProfileShare'
      { profileArn = pProfileArn_,
        sharedWith = pSharedWith_,
        clientRequestToken = pClientRequestToken_
      }

-- | The profile ARN.
createProfileShare_profileArn :: Lens.Lens' CreateProfileShare Prelude.Text
createProfileShare_profileArn = Lens.lens (\CreateProfileShare' {profileArn} -> profileArn) (\s@CreateProfileShare' {} a -> s {profileArn = a} :: CreateProfileShare)

-- | Undocumented member.
createProfileShare_sharedWith :: Lens.Lens' CreateProfileShare Prelude.Text
createProfileShare_sharedWith = Lens.lens (\CreateProfileShare' {sharedWith} -> sharedWith) (\s@CreateProfileShare' {} a -> s {sharedWith = a} :: CreateProfileShare)

-- | Undocumented member.
createProfileShare_clientRequestToken :: Lens.Lens' CreateProfileShare Prelude.Text
createProfileShare_clientRequestToken = Lens.lens (\CreateProfileShare' {clientRequestToken} -> clientRequestToken) (\s@CreateProfileShare' {} a -> s {clientRequestToken = a} :: CreateProfileShare)

instance Core.AWSRequest CreateProfileShare where
  type
    AWSResponse CreateProfileShare =
      CreateProfileShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileShareResponse'
            Prelude.<$> (x Data..?> "ProfileArn")
            Prelude.<*> (x Data..?> "ShareId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProfileShare where
  hashWithSalt _salt CreateProfileShare' {..} =
    _salt
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateProfileShare where
  rnf CreateProfileShare' {..} =
    Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateProfileShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProfileShare where
  toJSON CreateProfileShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SharedWith" Data..= sharedWith),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateProfileShare where
  toPath CreateProfileShare' {..} =
    Prelude.mconcat
      ["/profiles/", Data.toBS profileArn, "/shares"]

instance Data.ToQuery CreateProfileShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileShareResponse' smart constructor.
data CreateProfileShareResponse = CreateProfileShareResponse'
  { -- | The profile ARN.
    profileArn :: Prelude.Maybe Prelude.Text,
    shareId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'createProfileShareResponse_profileArn' - The profile ARN.
--
-- 'shareId', 'createProfileShareResponse_shareId' - Undocumented member.
--
-- 'httpStatus', 'createProfileShareResponse_httpStatus' - The response's http status code.
newCreateProfileShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProfileShareResponse
newCreateProfileShareResponse pHttpStatus_ =
  CreateProfileShareResponse'
    { profileArn =
        Prelude.Nothing,
      shareId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The profile ARN.
createProfileShareResponse_profileArn :: Lens.Lens' CreateProfileShareResponse (Prelude.Maybe Prelude.Text)
createProfileShareResponse_profileArn = Lens.lens (\CreateProfileShareResponse' {profileArn} -> profileArn) (\s@CreateProfileShareResponse' {} a -> s {profileArn = a} :: CreateProfileShareResponse)

-- | Undocumented member.
createProfileShareResponse_shareId :: Lens.Lens' CreateProfileShareResponse (Prelude.Maybe Prelude.Text)
createProfileShareResponse_shareId = Lens.lens (\CreateProfileShareResponse' {shareId} -> shareId) (\s@CreateProfileShareResponse' {} a -> s {shareId = a} :: CreateProfileShareResponse)

-- | The response's http status code.
createProfileShareResponse_httpStatus :: Lens.Lens' CreateProfileShareResponse Prelude.Int
createProfileShareResponse_httpStatus = Lens.lens (\CreateProfileShareResponse' {httpStatus} -> httpStatus) (\s@CreateProfileShareResponse' {} a -> s {httpStatus = a} :: CreateProfileShareResponse)

instance Prelude.NFData CreateProfileShareResponse where
  rnf CreateProfileShareResponse' {..} =
    Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf httpStatus
