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
-- Module      : Amazonka.APIGateway.GetSdkType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an SDK type.
module Amazonka.APIGateway.GetSdkType
  ( -- * Creating a Request
    GetSdkType (..),
    newGetSdkType,

    -- * Request Lenses
    getSdkType_id,

    -- * Destructuring the Response
    SdkType (..),
    newSdkType,

    -- * Response Lenses
    sdkType_configurationProperties,
    sdkType_description,
    sdkType_friendlyName,
    sdkType_id,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Get an SdkType instance.
--
-- /See:/ 'newGetSdkType' smart constructor.
data GetSdkType = GetSdkType'
  { -- | The identifier of the queried SdkType instance.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSdkType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getSdkType_id' - The identifier of the queried SdkType instance.
newGetSdkType ::
  -- | 'id'
  Prelude.Text ->
  GetSdkType
newGetSdkType pId_ = GetSdkType' {id = pId_}

-- | The identifier of the queried SdkType instance.
getSdkType_id :: Lens.Lens' GetSdkType Prelude.Text
getSdkType_id = Lens.lens (\GetSdkType' {id} -> id) (\s@GetSdkType' {} a -> s {id = a} :: GetSdkType)

instance Core.AWSRequest GetSdkType where
  type AWSResponse GetSdkType = SdkType
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetSdkType where
  hashWithSalt _salt GetSdkType' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetSdkType where
  rnf GetSdkType' {..} = Prelude.rnf id

instance Data.ToHeaders GetSdkType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetSdkType where
  toPath GetSdkType' {..} =
    Prelude.mconcat ["/sdktypes/", Data.toBS id]

instance Data.ToQuery GetSdkType where
  toQuery = Prelude.const Prelude.mempty
