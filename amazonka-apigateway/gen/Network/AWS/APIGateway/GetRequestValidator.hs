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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets a RequestValidator of a given RestApi.
--
-- /See:/ 'newGetRequestValidator' smart constructor.
data GetRequestValidator = GetRequestValidator'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the RequestValidator to be retrieved.
    requestValidatorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'requestValidatorId'
  Prelude.Text ->
  GetRequestValidator
newGetRequestValidator
  pRestApiId_
  pRequestValidatorId_ =
    GetRequestValidator'
      { restApiId = pRestApiId_,
        requestValidatorId = pRequestValidatorId_
      }

-- | [Required] The string identifier of the associated RestApi.
getRequestValidator_restApiId :: Lens.Lens' GetRequestValidator Prelude.Text
getRequestValidator_restApiId = Lens.lens (\GetRequestValidator' {restApiId} -> restApiId) (\s@GetRequestValidator' {} a -> s {restApiId = a} :: GetRequestValidator)

-- | [Required] The identifier of the RequestValidator to be retrieved.
getRequestValidator_requestValidatorId :: Lens.Lens' GetRequestValidator Prelude.Text
getRequestValidator_requestValidatorId = Lens.lens (\GetRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@GetRequestValidator' {} a -> s {requestValidatorId = a} :: GetRequestValidator)

instance Prelude.AWSRequest GetRequestValidator where
  type Rs GetRequestValidator = RequestValidator
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable GetRequestValidator

instance Prelude.NFData GetRequestValidator

instance Prelude.ToHeaders GetRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetRequestValidator where
  toPath GetRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/requestvalidators/",
        Prelude.toBS requestValidatorId
      ]

instance Prelude.ToQuery GetRequestValidator where
  toQuery = Prelude.const Prelude.mempty
