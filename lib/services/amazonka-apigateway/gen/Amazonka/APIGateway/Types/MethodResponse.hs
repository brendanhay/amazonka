{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Types.MethodResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.MethodResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a method response of a given HTTP status code returned to the
-- client. The method response is passed from the back end through the
-- associated integration response that can be transformed using a mapping
-- template.
--
-- /See:/ 'newMethodResponse' smart constructor.
data MethodResponse = MethodResponse'
  { -- | Specifies the Model resources used for the response\'s content-type.
    -- Response models are represented as a key\/value map, with a content-type
    -- as the key and a Model name as the value.
    responseModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A key-value map specifying required or optional response parameters that
    -- API Gateway can send back to the caller. A key defines a method response
    -- header and the value specifies whether the associated method response
    -- header is required or not. The expression of the key must match the
    -- pattern @method.response.header.{name}@, where @name@ is a valid and
    -- unique header name. API Gateway passes certain integration response data
    -- to the method response headers specified here according to the mapping
    -- you prescribe in the API\'s IntegrationResponse. The integration
    -- response data that can be mapped include an integration response header
    -- expressed in @integration.response.header.{name}@, a static value
    -- enclosed within a pair of single quotes (e.g., @\'application\/json\'@),
    -- or a JSON expression from the back-end response payload in the form of
    -- @integration.response.body.{JSON-expression}@, where @JSON-expression@
    -- is a valid JSON expression without the @$@ prefix.)
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool),
    -- | The method response\'s status code.
    statusCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseModels', 'methodResponse_responseModels' - Specifies the Model resources used for the response\'s content-type.
-- Response models are represented as a key\/value map, with a content-type
-- as the key and a Model name as the value.
--
-- 'responseParameters', 'methodResponse_responseParameters' - A key-value map specifying required or optional response parameters that
-- API Gateway can send back to the caller. A key defines a method response
-- header and the value specifies whether the associated method response
-- header is required or not. The expression of the key must match the
-- pattern @method.response.header.{name}@, where @name@ is a valid and
-- unique header name. API Gateway passes certain integration response data
-- to the method response headers specified here according to the mapping
-- you prescribe in the API\'s IntegrationResponse. The integration
-- response data that can be mapped include an integration response header
-- expressed in @integration.response.header.{name}@, a static value
-- enclosed within a pair of single quotes (e.g., @\'application\/json\'@),
-- or a JSON expression from the back-end response payload in the form of
-- @integration.response.body.{JSON-expression}@, where @JSON-expression@
-- is a valid JSON expression without the @$@ prefix.)
--
-- 'statusCode', 'methodResponse_statusCode' - The method response\'s status code.
newMethodResponse ::
  MethodResponse
newMethodResponse =
  MethodResponse'
    { responseModels = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Specifies the Model resources used for the response\'s content-type.
-- Response models are represented as a key\/value map, with a content-type
-- as the key and a Model name as the value.
methodResponse_responseModels :: Lens.Lens' MethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
methodResponse_responseModels = Lens.lens (\MethodResponse' {responseModels} -> responseModels) (\s@MethodResponse' {} a -> s {responseModels = a} :: MethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | A key-value map specifying required or optional response parameters that
-- API Gateway can send back to the caller. A key defines a method response
-- header and the value specifies whether the associated method response
-- header is required or not. The expression of the key must match the
-- pattern @method.response.header.{name}@, where @name@ is a valid and
-- unique header name. API Gateway passes certain integration response data
-- to the method response headers specified here according to the mapping
-- you prescribe in the API\'s IntegrationResponse. The integration
-- response data that can be mapped include an integration response header
-- expressed in @integration.response.header.{name}@, a static value
-- enclosed within a pair of single quotes (e.g., @\'application\/json\'@),
-- or a JSON expression from the back-end response payload in the form of
-- @integration.response.body.{JSON-expression}@, where @JSON-expression@
-- is a valid JSON expression without the @$@ prefix.)
methodResponse_responseParameters :: Lens.Lens' MethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
methodResponse_responseParameters = Lens.lens (\MethodResponse' {responseParameters} -> responseParameters) (\s@MethodResponse' {} a -> s {responseParameters = a} :: MethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | The method response\'s status code.
methodResponse_statusCode :: Lens.Lens' MethodResponse (Prelude.Maybe Prelude.Text)
methodResponse_statusCode = Lens.lens (\MethodResponse' {statusCode} -> statusCode) (\s@MethodResponse' {} a -> s {statusCode = a} :: MethodResponse)

instance Data.FromJSON MethodResponse where
  parseJSON =
    Data.withObject
      "MethodResponse"
      ( \x ->
          MethodResponse'
            Prelude.<$> (x Data..:? "responseModels" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "responseParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "statusCode")
      )

instance Prelude.Hashable MethodResponse where
  hashWithSalt _salt MethodResponse' {..} =
    _salt
      `Prelude.hashWithSalt` responseModels
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData MethodResponse where
  rnf MethodResponse' {..} =
    Prelude.rnf responseModels
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf statusCode
