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
-- Module      : Network.AWS.APIGateway.GetSdkType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.GetSdkType
  ( -- * Creating a Request
    GetSdkType (..),
    newGetSdkType,

    -- * Request Lenses
    getSdkType_id,

    -- * Destructuring the Response
    SdkType (..),
    newSdkType,

    -- * Response Lenses
    sdkType_friendlyName,
    sdkType_id,
    sdkType_configurationProperties,
    sdkType_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Get an SdkType instance.
--
-- /See:/ 'newGetSdkType' smart constructor.
data GetSdkType = GetSdkType'
  { -- | [Required] The identifier of the queried SdkType instance.
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
-- 'id', 'getSdkType_id' - [Required] The identifier of the queried SdkType instance.
newGetSdkType ::
  -- | 'id'
  Prelude.Text ->
  GetSdkType
newGetSdkType pId_ = GetSdkType' {id = pId_}

-- | [Required] The identifier of the queried SdkType instance.
getSdkType_id :: Lens.Lens' GetSdkType Prelude.Text
getSdkType_id = Lens.lens (\GetSdkType' {id} -> id) (\s@GetSdkType' {} a -> s {id = a} :: GetSdkType)

instance Core.AWSRequest GetSdkType where
  type AWSResponse GetSdkType = SdkType
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetSdkType

instance Prelude.NFData GetSdkType

instance Core.ToHeaders GetSdkType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetSdkType where
  toPath GetSdkType' {..} =
    Prelude.mconcat ["/sdktypes/", Core.toBS id]

instance Core.ToQuery GetSdkType where
  toQuery = Prelude.const Prelude.mempty
