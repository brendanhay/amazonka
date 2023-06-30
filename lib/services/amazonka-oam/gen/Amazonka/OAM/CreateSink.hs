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
-- Module      : Amazonka.OAM.CreateSink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this to create a /sink/ in the current account, so that it can be
-- used as a monitoring account in CloudWatch cross-account observability.
-- A sink is a resource that represents an attachment point in a monitoring
-- account. Source accounts can link to the sink to send observability
-- data.
--
-- After you create a sink, you must create a sink policy that allows
-- source accounts to attach to it. For more information, see
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_PutSinkPolicy.html PutSinkPolicy>.
--
-- Each account can contain one sink. If you delete a sink, you can then
-- create a new one in that account.
module Amazonka.OAM.CreateSink
  ( -- * Creating a Request
    CreateSink (..),
    newCreateSink,

    -- * Request Lenses
    createSink_tags,
    createSink_name,

    -- * Destructuring the Response
    CreateSinkResponse (..),
    newCreateSinkResponse,

    -- * Response Lenses
    createSinkResponse_arn,
    createSinkResponse_id,
    createSinkResponse_name,
    createSinkResponse_tags,
    createSinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSink' smart constructor.
data CreateSink = CreateSink'
  { -- | Assigns one or more tags (key-value pairs) to the link.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- For more information about using tags to control access, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the sink.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSink_tags' - Assigns one or more tags (key-value pairs) to the link.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- For more information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
--
-- 'name', 'createSink_name' - A name for the sink.
newCreateSink ::
  -- | 'name'
  Prelude.Text ->
  CreateSink
newCreateSink pName_ =
  CreateSink' {tags = Prelude.Nothing, name = pName_}

-- | Assigns one or more tags (key-value pairs) to the link.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- For more information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
createSink_tags :: Lens.Lens' CreateSink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSink_tags = Lens.lens (\CreateSink' {tags} -> tags) (\s@CreateSink' {} a -> s {tags = a} :: CreateSink) Prelude.. Lens.mapping Lens.coerced

-- | A name for the sink.
createSink_name :: Lens.Lens' CreateSink Prelude.Text
createSink_name = Lens.lens (\CreateSink' {name} -> name) (\s@CreateSink' {} a -> s {name = a} :: CreateSink)

instance Core.AWSRequest CreateSink where
  type AWSResponse CreateSink = CreateSinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSinkResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSink where
  hashWithSalt _salt CreateSink' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSink where
  rnf CreateSink' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateSink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSink where
  toJSON CreateSink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateSink where
  toPath = Prelude.const "/CreateSink"

instance Data.ToQuery CreateSink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSinkResponse' smart constructor.
data CreateSinkResponse = CreateSinkResponse'
  { -- | The ARN of the sink that is newly created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- sink ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the sink.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the sink.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createSinkResponse_arn' - The ARN of the sink that is newly created.
--
-- 'id', 'createSinkResponse_id' - The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
--
-- 'name', 'createSinkResponse_name' - The name of the sink.
--
-- 'tags', 'createSinkResponse_tags' - The tags assigned to the sink.
--
-- 'httpStatus', 'createSinkResponse_httpStatus' - The response's http status code.
newCreateSinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSinkResponse
newCreateSinkResponse pHttpStatus_ =
  CreateSinkResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the sink that is newly created.
createSinkResponse_arn :: Lens.Lens' CreateSinkResponse (Prelude.Maybe Prelude.Text)
createSinkResponse_arn = Lens.lens (\CreateSinkResponse' {arn} -> arn) (\s@CreateSinkResponse' {} a -> s {arn = a} :: CreateSinkResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
createSinkResponse_id :: Lens.Lens' CreateSinkResponse (Prelude.Maybe Prelude.Text)
createSinkResponse_id = Lens.lens (\CreateSinkResponse' {id} -> id) (\s@CreateSinkResponse' {} a -> s {id = a} :: CreateSinkResponse)

-- | The name of the sink.
createSinkResponse_name :: Lens.Lens' CreateSinkResponse (Prelude.Maybe Prelude.Text)
createSinkResponse_name = Lens.lens (\CreateSinkResponse' {name} -> name) (\s@CreateSinkResponse' {} a -> s {name = a} :: CreateSinkResponse)

-- | The tags assigned to the sink.
createSinkResponse_tags :: Lens.Lens' CreateSinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSinkResponse_tags = Lens.lens (\CreateSinkResponse' {tags} -> tags) (\s@CreateSinkResponse' {} a -> s {tags = a} :: CreateSinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSinkResponse_httpStatus :: Lens.Lens' CreateSinkResponse Prelude.Int
createSinkResponse_httpStatus = Lens.lens (\CreateSinkResponse' {httpStatus} -> httpStatus) (\s@CreateSinkResponse' {} a -> s {httpStatus = a} :: CreateSinkResponse)

instance Prelude.NFData CreateSinkResponse where
  rnf CreateSinkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
