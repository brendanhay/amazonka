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
-- Module      : Amazonka.NetworkManager.CreateGlobalNetwork
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty global network.
module Amazonka.NetworkManager.CreateGlobalNetwork
  ( -- * Creating a Request
    CreateGlobalNetwork (..),
    newCreateGlobalNetwork,

    -- * Request Lenses
    createGlobalNetwork_tags,
    createGlobalNetwork_description,

    -- * Destructuring the Response
    CreateGlobalNetworkResponse (..),
    newCreateGlobalNetworkResponse,

    -- * Response Lenses
    createGlobalNetworkResponse_globalNetwork,
    createGlobalNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGlobalNetwork' smart constructor.
data CreateGlobalNetwork = CreateGlobalNetwork'
  { -- | The tags to apply to the resource during creation.
    tags :: Prelude.Maybe [Tag],
    -- | A description of the global network.
    --
    -- Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGlobalNetwork_tags' - The tags to apply to the resource during creation.
--
-- 'description', 'createGlobalNetwork_description' - A description of the global network.
--
-- Constraints: Maximum length of 256 characters.
newCreateGlobalNetwork ::
  CreateGlobalNetwork
newCreateGlobalNetwork =
  CreateGlobalNetwork'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The tags to apply to the resource during creation.
createGlobalNetwork_tags :: Lens.Lens' CreateGlobalNetwork (Prelude.Maybe [Tag])
createGlobalNetwork_tags = Lens.lens (\CreateGlobalNetwork' {tags} -> tags) (\s@CreateGlobalNetwork' {} a -> s {tags = a} :: CreateGlobalNetwork) Prelude.. Lens.mapping Lens.coerced

-- | A description of the global network.
--
-- Constraints: Maximum length of 256 characters.
createGlobalNetwork_description :: Lens.Lens' CreateGlobalNetwork (Prelude.Maybe Prelude.Text)
createGlobalNetwork_description = Lens.lens (\CreateGlobalNetwork' {description} -> description) (\s@CreateGlobalNetwork' {} a -> s {description = a} :: CreateGlobalNetwork)

instance Core.AWSRequest CreateGlobalNetwork where
  type
    AWSResponse CreateGlobalNetwork =
      CreateGlobalNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGlobalNetworkResponse'
            Prelude.<$> (x Data..?> "GlobalNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGlobalNetwork where
  hashWithSalt _salt CreateGlobalNetwork' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateGlobalNetwork where
  rnf CreateGlobalNetwork' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders CreateGlobalNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGlobalNetwork where
  toJSON CreateGlobalNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Description" Data..=) Prelude.<$> description
          ]
      )

instance Data.ToPath CreateGlobalNetwork where
  toPath = Prelude.const "/global-networks"

instance Data.ToQuery CreateGlobalNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGlobalNetworkResponse' smart constructor.
data CreateGlobalNetworkResponse = CreateGlobalNetworkResponse'
  { -- | Information about the global network object.
    globalNetwork :: Prelude.Maybe GlobalNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetwork', 'createGlobalNetworkResponse_globalNetwork' - Information about the global network object.
--
-- 'httpStatus', 'createGlobalNetworkResponse_httpStatus' - The response's http status code.
newCreateGlobalNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGlobalNetworkResponse
newCreateGlobalNetworkResponse pHttpStatus_ =
  CreateGlobalNetworkResponse'
    { globalNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the global network object.
createGlobalNetworkResponse_globalNetwork :: Lens.Lens' CreateGlobalNetworkResponse (Prelude.Maybe GlobalNetwork)
createGlobalNetworkResponse_globalNetwork = Lens.lens (\CreateGlobalNetworkResponse' {globalNetwork} -> globalNetwork) (\s@CreateGlobalNetworkResponse' {} a -> s {globalNetwork = a} :: CreateGlobalNetworkResponse)

-- | The response's http status code.
createGlobalNetworkResponse_httpStatus :: Lens.Lens' CreateGlobalNetworkResponse Prelude.Int
createGlobalNetworkResponse_httpStatus = Lens.lens (\CreateGlobalNetworkResponse' {httpStatus} -> httpStatus) (\s@CreateGlobalNetworkResponse' {} a -> s {httpStatus = a} :: CreateGlobalNetworkResponse)

instance Prelude.NFData CreateGlobalNetworkResponse where
  rnf CreateGlobalNetworkResponse' {..} =
    Prelude.rnf globalNetwork
      `Prelude.seq` Prelude.rnf httpStatus
