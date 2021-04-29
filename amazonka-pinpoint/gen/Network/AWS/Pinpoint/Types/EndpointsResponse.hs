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
-- Module      : Network.AWS.Pinpoint.Types.EndpointsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointResponse
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about all the endpoints that are associated with a
-- user ID.
--
-- /See:/ 'newEndpointsResponse' smart constructor.
data EndpointsResponse = EndpointsResponse'
  { -- | An array of responses, one for each endpoint that\'s associated with the
    -- user ID.
    item :: [EndpointResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'item', 'endpointsResponse_item' - An array of responses, one for each endpoint that\'s associated with the
-- user ID.
newEndpointsResponse ::
  EndpointsResponse
newEndpointsResponse =
  EndpointsResponse' {item = Prelude.mempty}

-- | An array of responses, one for each endpoint that\'s associated with the
-- user ID.
endpointsResponse_item :: Lens.Lens' EndpointsResponse [EndpointResponse]
endpointsResponse_item = Lens.lens (\EndpointsResponse' {item} -> item) (\s@EndpointsResponse' {} a -> s {item = a} :: EndpointsResponse) Prelude.. Prelude._Coerce

instance Prelude.FromJSON EndpointsResponse where
  parseJSON =
    Prelude.withObject
      "EndpointsResponse"
      ( \x ->
          EndpointsResponse'
            Prelude.<$> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable EndpointsResponse

instance Prelude.NFData EndpointsResponse
