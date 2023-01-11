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
-- Module      : Amazonka.MemoryDb.CreateACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Access Control List. For more information, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/clusters.acls.html Authenticating users with Access Contol Lists (ACLs)>.
module Amazonka.MemoryDb.CreateACL
  ( -- * Creating a Request
    CreateACL (..),
    newCreateACL,

    -- * Request Lenses
    createACL_tags,
    createACL_userNames,
    createACL_aCLName,

    -- * Destructuring the Response
    CreateACLResponse (..),
    newCreateACLResponse,

    -- * Response Lenses
    createACLResponse_acl,
    createACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateACL' smart constructor.
data CreateACL = CreateACL'
  { -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The list of users that belong to the Access Control List.
    userNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the Access Control List.
    aCLName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createACL_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'userNames', 'createACL_userNames' - The list of users that belong to the Access Control List.
--
-- 'aCLName', 'createACL_aCLName' - The name of the Access Control List.
newCreateACL ::
  -- | 'aCLName'
  Prelude.Text ->
  CreateACL
newCreateACL pACLName_ =
  CreateACL'
    { tags = Prelude.Nothing,
      userNames = Prelude.Nothing,
      aCLName = pACLName_
    }

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createACL_tags :: Lens.Lens' CreateACL (Prelude.Maybe [Tag])
createACL_tags = Lens.lens (\CreateACL' {tags} -> tags) (\s@CreateACL' {} a -> s {tags = a} :: CreateACL) Prelude.. Lens.mapping Lens.coerced

-- | The list of users that belong to the Access Control List.
createACL_userNames :: Lens.Lens' CreateACL (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createACL_userNames = Lens.lens (\CreateACL' {userNames} -> userNames) (\s@CreateACL' {} a -> s {userNames = a} :: CreateACL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Access Control List.
createACL_aCLName :: Lens.Lens' CreateACL Prelude.Text
createACL_aCLName = Lens.lens (\CreateACL' {aCLName} -> aCLName) (\s@CreateACL' {} a -> s {aCLName = a} :: CreateACL)

instance Core.AWSRequest CreateACL where
  type AWSResponse CreateACL = CreateACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateACLResponse'
            Prelude.<$> (x Data..?> "ACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateACL where
  hashWithSalt _salt CreateACL' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userNames
      `Prelude.hashWithSalt` aCLName

instance Prelude.NFData CreateACL where
  rnf CreateACL' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userNames
      `Prelude.seq` Prelude.rnf aCLName

instance Data.ToHeaders CreateACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonMemoryDB.CreateACL" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateACL where
  toJSON CreateACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("UserNames" Data..=) Prelude.<$> userNames,
            Prelude.Just ("ACLName" Data..= aCLName)
          ]
      )

instance Data.ToPath CreateACL where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateACLResponse' smart constructor.
data CreateACLResponse = CreateACLResponse'
  { -- | The newly-created Access Control List.
    acl :: Prelude.Maybe ACL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'createACLResponse_acl' - The newly-created Access Control List.
--
-- 'httpStatus', 'createACLResponse_httpStatus' - The response's http status code.
newCreateACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateACLResponse
newCreateACLResponse pHttpStatus_ =
  CreateACLResponse'
    { acl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly-created Access Control List.
createACLResponse_acl :: Lens.Lens' CreateACLResponse (Prelude.Maybe ACL)
createACLResponse_acl = Lens.lens (\CreateACLResponse' {acl} -> acl) (\s@CreateACLResponse' {} a -> s {acl = a} :: CreateACLResponse)

-- | The response's http status code.
createACLResponse_httpStatus :: Lens.Lens' CreateACLResponse Prelude.Int
createACLResponse_httpStatus = Lens.lens (\CreateACLResponse' {httpStatus} -> httpStatus) (\s@CreateACLResponse' {} a -> s {httpStatus = a} :: CreateACLResponse)

instance Prelude.NFData CreateACLResponse where
  rnf CreateACLResponse' {..} =
    Prelude.rnf acl
      `Prelude.seq` Prelude.rnf httpStatus
