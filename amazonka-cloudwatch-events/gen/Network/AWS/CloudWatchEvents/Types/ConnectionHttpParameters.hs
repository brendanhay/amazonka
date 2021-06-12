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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionBodyParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionHeaderParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionQueryStringParameter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains additional parameters for the connection.
--
-- /See:/ 'newConnectionHttpParameters' smart constructor.
data ConnectionHttpParameters = ConnectionHttpParameters'
  { -- | Contains additional query string parameters for the connection.
    queryStringParameters :: Core.Maybe [ConnectionQueryStringParameter],
    -- | Contains additional header parameters for the connection.
    headerParameters :: Core.Maybe [ConnectionHeaderParameter],
    -- | Contains additional body string parameters for the connection.
    bodyParameters :: Core.Maybe [ConnectionBodyParameter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionHttpParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStringParameters', 'connectionHttpParameters_queryStringParameters' - Contains additional query string parameters for the connection.
--
-- 'headerParameters', 'connectionHttpParameters_headerParameters' - Contains additional header parameters for the connection.
--
-- 'bodyParameters', 'connectionHttpParameters_bodyParameters' - Contains additional body string parameters for the connection.
newConnectionHttpParameters ::
  ConnectionHttpParameters
newConnectionHttpParameters =
  ConnectionHttpParameters'
    { queryStringParameters =
        Core.Nothing,
      headerParameters = Core.Nothing,
      bodyParameters = Core.Nothing
    }

-- | Contains additional query string parameters for the connection.
connectionHttpParameters_queryStringParameters :: Lens.Lens' ConnectionHttpParameters (Core.Maybe [ConnectionQueryStringParameter])
connectionHttpParameters_queryStringParameters = Lens.lens (\ConnectionHttpParameters' {queryStringParameters} -> queryStringParameters) (\s@ConnectionHttpParameters' {} a -> s {queryStringParameters = a} :: ConnectionHttpParameters) Core.. Lens.mapping Lens._Coerce

-- | Contains additional header parameters for the connection.
connectionHttpParameters_headerParameters :: Lens.Lens' ConnectionHttpParameters (Core.Maybe [ConnectionHeaderParameter])
connectionHttpParameters_headerParameters = Lens.lens (\ConnectionHttpParameters' {headerParameters} -> headerParameters) (\s@ConnectionHttpParameters' {} a -> s {headerParameters = a} :: ConnectionHttpParameters) Core.. Lens.mapping Lens._Coerce

-- | Contains additional body string parameters for the connection.
connectionHttpParameters_bodyParameters :: Lens.Lens' ConnectionHttpParameters (Core.Maybe [ConnectionBodyParameter])
connectionHttpParameters_bodyParameters = Lens.lens (\ConnectionHttpParameters' {bodyParameters} -> bodyParameters) (\s@ConnectionHttpParameters' {} a -> s {bodyParameters = a} :: ConnectionHttpParameters) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ConnectionHttpParameters where
  parseJSON =
    Core.withObject
      "ConnectionHttpParameters"
      ( \x ->
          ConnectionHttpParameters'
            Core.<$> ( x Core..:? "QueryStringParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "HeaderParameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "BodyParameters" Core..!= Core.mempty)
      )

instance Core.Hashable ConnectionHttpParameters

instance Core.NFData ConnectionHttpParameters

instance Core.ToJSON ConnectionHttpParameters where
  toJSON ConnectionHttpParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("QueryStringParameters" Core..=)
              Core.<$> queryStringParameters,
            ("HeaderParameters" Core..=)
              Core.<$> headerParameters,
            ("BodyParameters" Core..=) Core.<$> bodyParameters
          ]
      )
