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
-- Module      : Network.AWS.MediaLive.CreateMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new multiplex.
module Network.AWS.MediaLive.CreateMultiplex
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a multiplex.
--
-- /See:/ 'newCreateMultiplex'' smart constructor.
data CreateMultiplex' = CreateMultiplex''
  { -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Unique request ID. This prevents retries from creating multiple
    -- resources.
    requestId :: Core.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: MultiplexSettings,
    -- | A list of availability zones for the multiplex. You must specify exactly
    -- two.
    availabilityZones :: [Core.Text],
    -- | Name of multiplex.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'multiplexSettings'
  MultiplexSettings ->
  -- | 'name'
  Core.Text ->
  CreateMultiplex'
newCreateMultiplex'
  pRequestId_
  pMultiplexSettings_
  pName_ =
    CreateMultiplex''
      { tags = Core.Nothing,
        requestId = pRequestId_,
        multiplexSettings = pMultiplexSettings_,
        availabilityZones = Core.mempty,
        name = pName_
      }

-- | A collection of key-value pairs.
createMultiplex'_tags :: Lens.Lens' CreateMultiplex' (Core.Maybe (Core.HashMap Core.Text Core.Text))
createMultiplex'_tags = Lens.lens (\CreateMultiplex'' {tags} -> tags) (\s@CreateMultiplex'' {} a -> s {tags = a} :: CreateMultiplex') Core.. Lens.mapping Lens._Coerce

-- | Unique request ID. This prevents retries from creating multiple
-- resources.
createMultiplex'_requestId :: Lens.Lens' CreateMultiplex' Core.Text
createMultiplex'_requestId = Lens.lens (\CreateMultiplex'' {requestId} -> requestId) (\s@CreateMultiplex'' {} a -> s {requestId = a} :: CreateMultiplex')

-- | Configuration for a multiplex event.
createMultiplex'_multiplexSettings :: Lens.Lens' CreateMultiplex' MultiplexSettings
createMultiplex'_multiplexSettings = Lens.lens (\CreateMultiplex'' {multiplexSettings} -> multiplexSettings) (\s@CreateMultiplex'' {} a -> s {multiplexSettings = a} :: CreateMultiplex')

-- | A list of availability zones for the multiplex. You must specify exactly
-- two.
createMultiplex'_availabilityZones :: Lens.Lens' CreateMultiplex' [Core.Text]
createMultiplex'_availabilityZones = Lens.lens (\CreateMultiplex'' {availabilityZones} -> availabilityZones) (\s@CreateMultiplex'' {} a -> s {availabilityZones = a} :: CreateMultiplex') Core.. Lens._Coerce

-- | Name of multiplex.
createMultiplex'_name :: Lens.Lens' CreateMultiplex' Core.Text
createMultiplex'_name = Lens.lens (\CreateMultiplex'' {name} -> name) (\s@CreateMultiplex'' {} a -> s {name = a} :: CreateMultiplex')

instance Core.AWSRequest CreateMultiplex' where
  type
    AWSResponse CreateMultiplex' =
      CreateMultiplexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMultiplexResponse'
            Core.<$> (x Core..?> "multiplex")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateMultiplex'

instance Core.NFData CreateMultiplex'

instance Core.ToHeaders CreateMultiplex' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateMultiplex' where
  toJSON CreateMultiplex'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("requestId" Core..= requestId),
            Core.Just
              ("multiplexSettings" Core..= multiplexSettings),
            Core.Just
              ("availabilityZones" Core..= availabilityZones),
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateMultiplex' where
  toPath = Core.const "/prod/multiplexes"

instance Core.ToQuery CreateMultiplex' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for CreateMultiplexResponse
--
-- /See:/ 'newCreateMultiplexResponse' smart constructor.
data CreateMultiplexResponse = CreateMultiplexResponse'
  { -- | The newly created multiplex.
    multiplex :: Core.Maybe Multiplex,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateMultiplexResponse
newCreateMultiplexResponse pHttpStatus_ =
  CreateMultiplexResponse'
    { multiplex = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created multiplex.
createMultiplexResponse_multiplex :: Lens.Lens' CreateMultiplexResponse (Core.Maybe Multiplex)
createMultiplexResponse_multiplex = Lens.lens (\CreateMultiplexResponse' {multiplex} -> multiplex) (\s@CreateMultiplexResponse' {} a -> s {multiplex = a} :: CreateMultiplexResponse)

-- | The response's http status code.
createMultiplexResponse_httpStatus :: Lens.Lens' CreateMultiplexResponse Core.Int
createMultiplexResponse_httpStatus = Lens.lens (\CreateMultiplexResponse' {httpStatus} -> httpStatus) (\s@CreateMultiplexResponse' {} a -> s {httpStatus = a} :: CreateMultiplexResponse)

instance Core.NFData CreateMultiplexResponse
