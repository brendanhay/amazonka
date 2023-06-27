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
-- Module      : Amazonka.SecurityLake.CreateAwsLogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a natively supported Amazon Web Service as an Amazon Security Lake
-- source. Enables source types for member accounts in required Amazon Web
-- Services Regions, based on the parameters you specify. You can choose
-- any source type in any Region for either accounts that are part of a
-- trusted organization or standalone accounts. Once you add an Amazon Web
-- Service as a source, Security Lake starts collecting logs and events
-- from it,
--
-- You can use this API only to enable natively supported Amazon Web
-- Services as a source. Use @CreateCustomLogSource@ to enable data
-- collection from a custom source.
module Amazonka.SecurityLake.CreateAwsLogSource
  ( -- * Creating a Request
    CreateAwsLogSource (..),
    newCreateAwsLogSource,

    -- * Request Lenses
    createAwsLogSource_sources,

    -- * Destructuring the Response
    CreateAwsLogSourceResponse (..),
    newCreateAwsLogSourceResponse,

    -- * Response Lenses
    createAwsLogSourceResponse_failed,
    createAwsLogSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateAwsLogSource' smart constructor.
data CreateAwsLogSource = CreateAwsLogSource'
  { -- | Specify the natively-supported Amazon Web Services service to add as a
    -- source in Security Lake.
    sources :: [AwsLogSourceConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAwsLogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'createAwsLogSource_sources' - Specify the natively-supported Amazon Web Services service to add as a
-- source in Security Lake.
newCreateAwsLogSource ::
  CreateAwsLogSource
newCreateAwsLogSource =
  CreateAwsLogSource' {sources = Prelude.mempty}

-- | Specify the natively-supported Amazon Web Services service to add as a
-- source in Security Lake.
createAwsLogSource_sources :: Lens.Lens' CreateAwsLogSource [AwsLogSourceConfiguration]
createAwsLogSource_sources = Lens.lens (\CreateAwsLogSource' {sources} -> sources) (\s@CreateAwsLogSource' {} a -> s {sources = a} :: CreateAwsLogSource) Prelude.. Lens.coerced

instance Core.AWSRequest CreateAwsLogSource where
  type
    AWSResponse CreateAwsLogSource =
      CreateAwsLogSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAwsLogSourceResponse'
            Prelude.<$> (x Data..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAwsLogSource where
  hashWithSalt _salt CreateAwsLogSource' {..} =
    _salt `Prelude.hashWithSalt` sources

instance Prelude.NFData CreateAwsLogSource where
  rnf CreateAwsLogSource' {..} = Prelude.rnf sources

instance Data.ToHeaders CreateAwsLogSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAwsLogSource where
  toJSON CreateAwsLogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("sources" Data..= sources)]
      )

instance Data.ToPath CreateAwsLogSource where
  toPath = Prelude.const "/v1/datalake/logsources/aws"

instance Data.ToQuery CreateAwsLogSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAwsLogSourceResponse' smart constructor.
data CreateAwsLogSourceResponse = CreateAwsLogSourceResponse'
  { -- | Lists all accounts in which enabling a natively supported Amazon Web
    -- Service as a Security Lake source failed. The failure occurred as these
    -- accounts are not part of an organization.
    failed :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAwsLogSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'createAwsLogSourceResponse_failed' - Lists all accounts in which enabling a natively supported Amazon Web
-- Service as a Security Lake source failed. The failure occurred as these
-- accounts are not part of an organization.
--
-- 'httpStatus', 'createAwsLogSourceResponse_httpStatus' - The response's http status code.
newCreateAwsLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAwsLogSourceResponse
newCreateAwsLogSourceResponse pHttpStatus_ =
  CreateAwsLogSourceResponse'
    { failed =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists all accounts in which enabling a natively supported Amazon Web
-- Service as a Security Lake source failed. The failure occurred as these
-- accounts are not part of an organization.
createAwsLogSourceResponse_failed :: Lens.Lens' CreateAwsLogSourceResponse (Prelude.Maybe [Prelude.Text])
createAwsLogSourceResponse_failed = Lens.lens (\CreateAwsLogSourceResponse' {failed} -> failed) (\s@CreateAwsLogSourceResponse' {} a -> s {failed = a} :: CreateAwsLogSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createAwsLogSourceResponse_httpStatus :: Lens.Lens' CreateAwsLogSourceResponse Prelude.Int
createAwsLogSourceResponse_httpStatus = Lens.lens (\CreateAwsLogSourceResponse' {httpStatus} -> httpStatus) (\s@CreateAwsLogSourceResponse' {} a -> s {httpStatus = a} :: CreateAwsLogSourceResponse)

instance Prelude.NFData CreateAwsLogSourceResponse where
  rnf CreateAwsLogSourceResponse' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf httpStatus
