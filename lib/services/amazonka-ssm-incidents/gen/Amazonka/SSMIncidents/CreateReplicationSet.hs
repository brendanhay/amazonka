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
-- Module      : Amazonka.SSMIncidents.CreateReplicationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A replication set replicates and encrypts your data to the provided
-- Regions with the provided KMS key.
module Amazonka.SSMIncidents.CreateReplicationSet
  ( -- * Creating a Request
    CreateReplicationSet (..),
    newCreateReplicationSet,

    -- * Request Lenses
    createReplicationSet_clientToken,
    createReplicationSet_tags,
    createReplicationSet_regions,

    -- * Destructuring the Response
    CreateReplicationSetResponse (..),
    newCreateReplicationSetResponse,

    -- * Response Lenses
    createReplicationSetResponse_httpStatus,
    createReplicationSetResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newCreateReplicationSet' smart constructor.
data CreateReplicationSet = CreateReplicationSet'
  { -- | A token that ensures that the operation is called only once with the
    -- specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to add to the replication set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Regions that Incident Manager replicates your data to. You can have
    -- up to three Regions in your replication set.
    regions :: Prelude.HashMap Prelude.Text RegionMapInputValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createReplicationSet_clientToken' - A token that ensures that the operation is called only once with the
-- specified details.
--
-- 'tags', 'createReplicationSet_tags' - A list of tags to add to the replication set.
--
-- 'regions', 'createReplicationSet_regions' - The Regions that Incident Manager replicates your data to. You can have
-- up to three Regions in your replication set.
newCreateReplicationSet ::
  CreateReplicationSet
newCreateReplicationSet =
  CreateReplicationSet'
    { clientToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      regions = Prelude.mempty
    }

-- | A token that ensures that the operation is called only once with the
-- specified details.
createReplicationSet_clientToken :: Lens.Lens' CreateReplicationSet (Prelude.Maybe Prelude.Text)
createReplicationSet_clientToken = Lens.lens (\CreateReplicationSet' {clientToken} -> clientToken) (\s@CreateReplicationSet' {} a -> s {clientToken = a} :: CreateReplicationSet)

-- | A list of tags to add to the replication set.
createReplicationSet_tags :: Lens.Lens' CreateReplicationSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createReplicationSet_tags = Lens.lens (\CreateReplicationSet' {tags} -> tags) (\s@CreateReplicationSet' {} a -> s {tags = a} :: CreateReplicationSet) Prelude.. Lens.mapping Lens.coerced

-- | The Regions that Incident Manager replicates your data to. You can have
-- up to three Regions in your replication set.
createReplicationSet_regions :: Lens.Lens' CreateReplicationSet (Prelude.HashMap Prelude.Text RegionMapInputValue)
createReplicationSet_regions = Lens.lens (\CreateReplicationSet' {regions} -> regions) (\s@CreateReplicationSet' {} a -> s {regions = a} :: CreateReplicationSet) Prelude.. Lens.coerced

instance Core.AWSRequest CreateReplicationSet where
  type
    AWSResponse CreateReplicationSet =
      CreateReplicationSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable CreateReplicationSet where
  hashWithSalt _salt CreateReplicationSet' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` regions

instance Prelude.NFData CreateReplicationSet where
  rnf CreateReplicationSet' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf regions

instance Data.ToHeaders CreateReplicationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateReplicationSet where
  toJSON CreateReplicationSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("regions" Data..= regions)
          ]
      )

instance Data.ToPath CreateReplicationSet where
  toPath = Prelude.const "/createReplicationSet"

instance Data.ToQuery CreateReplicationSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReplicationSetResponse' smart constructor.
data CreateReplicationSetResponse = CreateReplicationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the replication set.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createReplicationSetResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createReplicationSetResponse_arn' - The Amazon Resource Name (ARN) of the replication set.
newCreateReplicationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  CreateReplicationSetResponse
newCreateReplicationSetResponse pHttpStatus_ pArn_ =
  CreateReplicationSetResponse'
    { httpStatus =
        pHttpStatus_,
      arn = pArn_
    }

-- | The response's http status code.
createReplicationSetResponse_httpStatus :: Lens.Lens' CreateReplicationSetResponse Prelude.Int
createReplicationSetResponse_httpStatus = Lens.lens (\CreateReplicationSetResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationSetResponse' {} a -> s {httpStatus = a} :: CreateReplicationSetResponse)

-- | The Amazon Resource Name (ARN) of the replication set.
createReplicationSetResponse_arn :: Lens.Lens' CreateReplicationSetResponse Prelude.Text
createReplicationSetResponse_arn = Lens.lens (\CreateReplicationSetResponse' {arn} -> arn) (\s@CreateReplicationSetResponse' {} a -> s {arn = a} :: CreateReplicationSetResponse)

instance Prelude.NFData CreateReplicationSetResponse where
  rnf CreateReplicationSetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
