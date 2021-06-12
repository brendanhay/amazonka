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
-- Module      : Network.AWS.APIGateway.GetRequestValidator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a RequestValidator of a given RestApi.
module Network.AWS.APIGateway.GetRequestValidator
  ( -- * Creating a Request
    GetRequestValidator (..),
    newGetRequestValidator,

    -- * Request Lenses
    getRequestValidator_restApiId,
    getRequestValidator_requestValidatorId,

    -- * Destructuring the Response
    RequestValidator (..),
    newRequestValidator,

    -- * Response Lenses
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets a RequestValidator of a given RestApi.
--
-- /See:/ 'newGetRequestValidator' smart constructor.
data GetRequestValidator = GetRequestValidator'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The identifier of the RequestValidator to be retrieved.
    requestValidatorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getRequestValidator_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'requestValidatorId', 'getRequestValidator_requestValidatorId' - [Required] The identifier of the RequestValidator to be retrieved.
newGetRequestValidator ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'requestValidatorId'
  Core.Text ->
  GetRequestValidator
newGetRequestValidator
  pRestApiId_
  pRequestValidatorId_ =
    GetRequestValidator'
      { restApiId = pRestApiId_,
        requestValidatorId = pRequestValidatorId_
      }

-- | [Required] The string identifier of the associated RestApi.
getRequestValidator_restApiId :: Lens.Lens' GetRequestValidator Core.Text
getRequestValidator_restApiId = Lens.lens (\GetRequestValidator' {restApiId} -> restApiId) (\s@GetRequestValidator' {} a -> s {restApiId = a} :: GetRequestValidator)

-- | [Required] The identifier of the RequestValidator to be retrieved.
getRequestValidator_requestValidatorId :: Lens.Lens' GetRequestValidator Core.Text
getRequestValidator_requestValidatorId = Lens.lens (\GetRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@GetRequestValidator' {} a -> s {requestValidatorId = a} :: GetRequestValidator)

instance Core.AWSRequest GetRequestValidator where
  type
    AWSResponse GetRequestValidator =
      RequestValidator
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetRequestValidator

instance Core.NFData GetRequestValidator

instance Core.ToHeaders GetRequestValidator where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetRequestValidator where
  toPath GetRequestValidator' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/requestvalidators/",
        Core.toBS requestValidatorId
      ]

instance Core.ToQuery GetRequestValidator where
  toQuery = Core.const Core.mempty
