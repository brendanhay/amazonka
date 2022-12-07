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
-- Module      : Amazonka.Pinpoint.Types.EndpointsResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EndpointResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about all the endpoints that are associated with a
-- user ID.
--
-- /See:/ 'newEndpointsResponse' smart constructor.
data EndpointsResponse = EndpointsResponse'
  { -- | An array of responses, one for each endpoint that\'s associated with the
    -- user ID.
    item :: [EndpointResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
endpointsResponse_item = Lens.lens (\EndpointsResponse' {item} -> item) (\s@EndpointsResponse' {} a -> s {item = a} :: EndpointsResponse) Prelude.. Lens.coerced

instance Data.FromJSON EndpointsResponse where
  parseJSON =
    Data.withObject
      "EndpointsResponse"
      ( \x ->
          EndpointsResponse'
            Prelude.<$> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EndpointsResponse where
  hashWithSalt _salt EndpointsResponse' {..} =
    _salt `Prelude.hashWithSalt` item

instance Prelude.NFData EndpointsResponse where
  rnf EndpointsResponse' {..} = Prelude.rnf item
