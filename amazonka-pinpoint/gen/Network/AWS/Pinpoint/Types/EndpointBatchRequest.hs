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
-- Module      : Network.AWS.Pinpoint.Types.EndpointBatchRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointBatchRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointBatchItem
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a batch of endpoints to create or update and the settings and
-- attributes to set or change for each endpoint.
--
-- /See:/ 'newEndpointBatchRequest' smart constructor.
data EndpointBatchRequest = EndpointBatchRequest'
  { -- | An array that defines the endpoints to create or update and, for each
    -- endpoint, the property values to set or change. An array can contain a
    -- maximum of 100 items.
    item :: [EndpointBatchItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointBatchRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'item', 'endpointBatchRequest_item' - An array that defines the endpoints to create or update and, for each
-- endpoint, the property values to set or change. An array can contain a
-- maximum of 100 items.
newEndpointBatchRequest ::
  EndpointBatchRequest
newEndpointBatchRequest =
  EndpointBatchRequest' {item = Prelude.mempty}

-- | An array that defines the endpoints to create or update and, for each
-- endpoint, the property values to set or change. An array can contain a
-- maximum of 100 items.
endpointBatchRequest_item :: Lens.Lens' EndpointBatchRequest [EndpointBatchItem]
endpointBatchRequest_item = Lens.lens (\EndpointBatchRequest' {item} -> item) (\s@EndpointBatchRequest' {} a -> s {item = a} :: EndpointBatchRequest) Prelude.. Prelude._Coerce

instance Prelude.Hashable EndpointBatchRequest

instance Prelude.NFData EndpointBatchRequest

instance Prelude.ToJSON EndpointBatchRequest where
  toJSON EndpointBatchRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Item" Prelude..= item)]
      )
