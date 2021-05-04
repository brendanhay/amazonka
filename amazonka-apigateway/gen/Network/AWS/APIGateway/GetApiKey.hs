{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.GetApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current ApiKey resource.
module Network.AWS.APIGateway.GetApiKey
  ( -- * Creating a Request
    GetApiKey (..),
    newGetApiKey,

    -- * Request Lenses
    getApiKey_includeValue,
    getApiKey_apiKey,

    -- * Destructuring the Response
    ApiKey (..),
    newApiKey,

    -- * Response Lenses
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about the current ApiKey resource.
--
-- /See:/ 'newGetApiKey' smart constructor.
data GetApiKey = GetApiKey'
  { -- | A boolean flag to specify whether (@true@) or not (@false@) the result
    -- contains the key value.
    includeValue :: Prelude.Maybe Prelude.Bool,
    -- | [Required] The identifier of the ApiKey resource.
    apiKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeValue', 'getApiKey_includeValue' - A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains the key value.
--
-- 'apiKey', 'getApiKey_apiKey' - [Required] The identifier of the ApiKey resource.
newGetApiKey ::
  -- | 'apiKey'
  Prelude.Text ->
  GetApiKey
newGetApiKey pApiKey_ =
  GetApiKey'
    { includeValue = Prelude.Nothing,
      apiKey = pApiKey_
    }

-- | A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains the key value.
getApiKey_includeValue :: Lens.Lens' GetApiKey (Prelude.Maybe Prelude.Bool)
getApiKey_includeValue = Lens.lens (\GetApiKey' {includeValue} -> includeValue) (\s@GetApiKey' {} a -> s {includeValue = a} :: GetApiKey)

-- | [Required] The identifier of the ApiKey resource.
getApiKey_apiKey :: Lens.Lens' GetApiKey Prelude.Text
getApiKey_apiKey = Lens.lens (\GetApiKey' {apiKey} -> apiKey) (\s@GetApiKey' {} a -> s {apiKey = a} :: GetApiKey)

instance Prelude.AWSRequest GetApiKey where
  type Rs GetApiKey = ApiKey
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable GetApiKey

instance Prelude.NFData GetApiKey

instance Prelude.ToHeaders GetApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetApiKey where
  toPath GetApiKey' {..} =
    Prelude.mconcat ["/apikeys/", Prelude.toBS apiKey]

instance Prelude.ToQuery GetApiKey where
  toQuery GetApiKey' {..} =
    Prelude.mconcat
      ["includeValue" Prelude.=: includeValue]
