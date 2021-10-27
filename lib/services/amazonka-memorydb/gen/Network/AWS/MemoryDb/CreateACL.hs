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
-- Module      : Network.AWS.MemoryDb.CreateACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Access Control List. For more information, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/clusters.acls.html Authenticating users with Access Contol Lists (ACLs)>.
module Network.AWS.MemoryDb.CreateACL
  ( -- * Creating a Request
    CreateACL (..),
    newCreateACL,

    -- * Request Lenses
    createACL_userNames,
    createACL_tags,
    createACL_aCLName,

    -- * Destructuring the Response
    CreateACLResponse (..),
    newCreateACLResponse,

    -- * Response Lenses
    createACLResponse_acl,
    createACLResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MemoryDb.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateACL' smart constructor.
data CreateACL = CreateACL'
  { -- | The list of users that belong to the Access Control List.
    userNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
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
-- 'userNames', 'createACL_userNames' - The list of users that belong to the Access Control List.
--
-- 'tags', 'createACL_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'aCLName', 'createACL_aCLName' - The name of the Access Control List.
newCreateACL ::
  -- | 'aCLName'
  Prelude.Text ->
  CreateACL
newCreateACL pACLName_ =
  CreateACL'
    { userNames = Prelude.Nothing,
      tags = Prelude.Nothing,
      aCLName = pACLName_
    }

-- | The list of users that belong to the Access Control List.
createACL_userNames :: Lens.Lens' CreateACL (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createACL_userNames = Lens.lens (\CreateACL' {userNames} -> userNames) (\s@CreateACL' {} a -> s {userNames = a} :: CreateACL) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createACL_tags :: Lens.Lens' CreateACL (Prelude.Maybe [Tag])
createACL_tags = Lens.lens (\CreateACL' {tags} -> tags) (\s@CreateACL' {} a -> s {tags = a} :: CreateACL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Access Control List.
createACL_aCLName :: Lens.Lens' CreateACL Prelude.Text
createACL_aCLName = Lens.lens (\CreateACL' {aCLName} -> aCLName) (\s@CreateACL' {} a -> s {aCLName = a} :: CreateACL)

instance Core.AWSRequest CreateACL where
  type AWSResponse CreateACL = CreateACLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateACLResponse'
            Prelude.<$> (x Core..?> "ACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateACL

instance Prelude.NFData CreateACL

instance Core.ToHeaders CreateACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonMemoryDB.CreateACL" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateACL where
  toJSON CreateACL' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserNames" Core..=) Prelude.<$> userNames,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("ACLName" Core..= aCLName)
          ]
      )

instance Core.ToPath CreateACL where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateACL where
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

instance Prelude.NFData CreateACLResponse
