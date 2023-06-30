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
-- Module      : Amazonka.MediaLive.CreateMultiplex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new multiplex.
module Amazonka.MediaLive.CreateMultiplex
  ( -- * Creating a Request
    CreateMultiplex' (..),
    newCreateMultiplex',

    -- * Request Lenses
    createMultiplex'_tags,
    createMultiplex'_requestId,
    createMultiplex'_multiplexSettings,
    createMultiplex'_availabilityZones,
    createMultiplex'_name,

    -- * Destructuring the Response
    CreateMultiplexResponse (..),
    newCreateMultiplexResponse,

    -- * Response Lenses
    createMultiplexResponse_multiplex,
    createMultiplexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to create a multiplex.
--
-- /See:/ 'newCreateMultiplex'' smart constructor.
data CreateMultiplex' = CreateMultiplex''
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique request ID. This prevents retries from creating multiple
    -- resources.
    requestId :: Prelude.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: MultiplexSettings,
    -- | A list of availability zones for the multiplex. You must specify exactly
    -- two.
    availabilityZones :: [Prelude.Text],
    -- | Name of multiplex.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultiplex'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMultiplex'_tags' - A collection of key-value pairs.
--
-- 'requestId', 'createMultiplex'_requestId' - Unique request ID. This prevents retries from creating multiple
-- resources.
--
-- 'multiplexSettings', 'createMultiplex'_multiplexSettings' - Configuration for a multiplex event.
--
-- 'availabilityZones', 'createMultiplex'_availabilityZones' - A list of availability zones for the multiplex. You must specify exactly
-- two.
--
-- 'name', 'createMultiplex'_name' - Name of multiplex.
newCreateMultiplex' ::
  -- | 'requestId'
  Prelude.Text ->
  -- | 'multiplexSettings'
  MultiplexSettings ->
  -- | 'name'
  Prelude.Text ->
  CreateMultiplex'
newCreateMultiplex'
  pRequestId_
  pMultiplexSettings_
  pName_ =
    CreateMultiplex''
      { tags = Prelude.Nothing,
        requestId = pRequestId_,
        multiplexSettings = pMultiplexSettings_,
        availabilityZones = Prelude.mempty,
        name = pName_
      }

-- | A collection of key-value pairs.
createMultiplex'_tags :: Lens.Lens' CreateMultiplex' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMultiplex'_tags = Lens.lens (\CreateMultiplex'' {tags} -> tags) (\s@CreateMultiplex'' {} a -> s {tags = a} :: CreateMultiplex') Prelude.. Lens.mapping Lens.coerced

-- | Unique request ID. This prevents retries from creating multiple
-- resources.
createMultiplex'_requestId :: Lens.Lens' CreateMultiplex' Prelude.Text
createMultiplex'_requestId = Lens.lens (\CreateMultiplex'' {requestId} -> requestId) (\s@CreateMultiplex'' {} a -> s {requestId = a} :: CreateMultiplex')

-- | Configuration for a multiplex event.
createMultiplex'_multiplexSettings :: Lens.Lens' CreateMultiplex' MultiplexSettings
createMultiplex'_multiplexSettings = Lens.lens (\CreateMultiplex'' {multiplexSettings} -> multiplexSettings) (\s@CreateMultiplex'' {} a -> s {multiplexSettings = a} :: CreateMultiplex')

-- | A list of availability zones for the multiplex. You must specify exactly
-- two.
createMultiplex'_availabilityZones :: Lens.Lens' CreateMultiplex' [Prelude.Text]
createMultiplex'_availabilityZones = Lens.lens (\CreateMultiplex'' {availabilityZones} -> availabilityZones) (\s@CreateMultiplex'' {} a -> s {availabilityZones = a} :: CreateMultiplex') Prelude.. Lens.coerced

-- | Name of multiplex.
createMultiplex'_name :: Lens.Lens' CreateMultiplex' Prelude.Text
createMultiplex'_name = Lens.lens (\CreateMultiplex'' {name} -> name) (\s@CreateMultiplex'' {} a -> s {name = a} :: CreateMultiplex')

instance Core.AWSRequest CreateMultiplex' where
  type
    AWSResponse CreateMultiplex' =
      CreateMultiplexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMultiplexResponse'
            Prelude.<$> (x Data..?> "multiplex")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMultiplex' where
  hashWithSalt _salt CreateMultiplex'' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` multiplexSettings
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateMultiplex' where
  rnf CreateMultiplex'' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf multiplexSettings
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateMultiplex' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMultiplex' where
  toJSON CreateMultiplex'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("requestId" Data..= requestId),
            Prelude.Just
              ("multiplexSettings" Data..= multiplexSettings),
            Prelude.Just
              ("availabilityZones" Data..= availabilityZones),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateMultiplex' where
  toPath = Prelude.const "/prod/multiplexes"

instance Data.ToQuery CreateMultiplex' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for CreateMultiplexResponse
--
-- /See:/ 'newCreateMultiplexResponse' smart constructor.
data CreateMultiplexResponse = CreateMultiplexResponse'
  { -- | The newly created multiplex.
    multiplex :: Prelude.Maybe Multiplex,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplex', 'createMultiplexResponse_multiplex' - The newly created multiplex.
--
-- 'httpStatus', 'createMultiplexResponse_httpStatus' - The response's http status code.
newCreateMultiplexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMultiplexResponse
newCreateMultiplexResponse pHttpStatus_ =
  CreateMultiplexResponse'
    { multiplex =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created multiplex.
createMultiplexResponse_multiplex :: Lens.Lens' CreateMultiplexResponse (Prelude.Maybe Multiplex)
createMultiplexResponse_multiplex = Lens.lens (\CreateMultiplexResponse' {multiplex} -> multiplex) (\s@CreateMultiplexResponse' {} a -> s {multiplex = a} :: CreateMultiplexResponse)

-- | The response's http status code.
createMultiplexResponse_httpStatus :: Lens.Lens' CreateMultiplexResponse Prelude.Int
createMultiplexResponse_httpStatus = Lens.lens (\CreateMultiplexResponse' {httpStatus} -> httpStatus) (\s@CreateMultiplexResponse' {} a -> s {httpStatus = a} :: CreateMultiplexResponse)

instance Prelude.NFData CreateMultiplexResponse where
  rnf CreateMultiplexResponse' {..} =
    Prelude.rnf multiplex
      `Prelude.seq` Prelude.rnf httpStatus
