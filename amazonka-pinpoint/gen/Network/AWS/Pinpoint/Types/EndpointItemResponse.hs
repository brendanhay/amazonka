{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.EndpointItemResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointItemResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the status code and message that result from processing data
-- for an endpoint.
--
-- /See:/ 'newEndpointItemResponse' smart constructor.
data EndpointItemResponse = EndpointItemResponse'
  { -- | The custom message that\'s returned in the response as a result of
    -- processing the endpoint data.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code that\'s returned in the response as a result of
    -- processing the endpoint data.
    statusCode :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'endpointItemResponse_message' - The custom message that\'s returned in the response as a result of
-- processing the endpoint data.
--
-- 'statusCode', 'endpointItemResponse_statusCode' - The status code that\'s returned in the response as a result of
-- processing the endpoint data.
newEndpointItemResponse ::
  EndpointItemResponse
newEndpointItemResponse =
  EndpointItemResponse'
    { message = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | The custom message that\'s returned in the response as a result of
-- processing the endpoint data.
endpointItemResponse_message :: Lens.Lens' EndpointItemResponse (Prelude.Maybe Prelude.Text)
endpointItemResponse_message = Lens.lens (\EndpointItemResponse' {message} -> message) (\s@EndpointItemResponse' {} a -> s {message = a} :: EndpointItemResponse)

-- | The status code that\'s returned in the response as a result of
-- processing the endpoint data.
endpointItemResponse_statusCode :: Lens.Lens' EndpointItemResponse (Prelude.Maybe Prelude.Int)
endpointItemResponse_statusCode = Lens.lens (\EndpointItemResponse' {statusCode} -> statusCode) (\s@EndpointItemResponse' {} a -> s {statusCode = a} :: EndpointItemResponse)

instance Prelude.FromJSON EndpointItemResponse where
  parseJSON =
    Prelude.withObject
      "EndpointItemResponse"
      ( \x ->
          EndpointItemResponse'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "StatusCode")
      )

instance Prelude.Hashable EndpointItemResponse

instance Prelude.NFData EndpointItemResponse
