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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains additional parameters for the connection.
--
-- /See:/ 'newConnectionHttpParameters' smart constructor.
data ConnectionHttpParameters = ConnectionHttpParameters'
  { -- | Contains additional query string parameters for the connection.
    queryStringParameters :: Prelude.Maybe [ConnectionQueryStringParameter],
    -- | Contains additional header parameters for the connection.
    headerParameters :: Prelude.Maybe [ConnectionHeaderParameter],
    -- | Contains additional body string parameters for the connection.
    bodyParameters :: Prelude.Maybe [ConnectionBodyParameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      headerParameters = Prelude.Nothing,
      bodyParameters = Prelude.Nothing
    }

-- | Contains additional query string parameters for the connection.
connectionHttpParameters_queryStringParameters :: Lens.Lens' ConnectionHttpParameters (Prelude.Maybe [ConnectionQueryStringParameter])
connectionHttpParameters_queryStringParameters = Lens.lens (\ConnectionHttpParameters' {queryStringParameters} -> queryStringParameters) (\s@ConnectionHttpParameters' {} a -> s {queryStringParameters = a} :: ConnectionHttpParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | Contains additional header parameters for the connection.
connectionHttpParameters_headerParameters :: Lens.Lens' ConnectionHttpParameters (Prelude.Maybe [ConnectionHeaderParameter])
connectionHttpParameters_headerParameters = Lens.lens (\ConnectionHttpParameters' {headerParameters} -> headerParameters) (\s@ConnectionHttpParameters' {} a -> s {headerParameters = a} :: ConnectionHttpParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | Contains additional body string parameters for the connection.
connectionHttpParameters_bodyParameters :: Lens.Lens' ConnectionHttpParameters (Prelude.Maybe [ConnectionBodyParameter])
connectionHttpParameters_bodyParameters = Lens.lens (\ConnectionHttpParameters' {bodyParameters} -> bodyParameters) (\s@ConnectionHttpParameters' {} a -> s {bodyParameters = a} :: ConnectionHttpParameters) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ConnectionHttpParameters where
  parseJSON =
    Prelude.withObject
      "ConnectionHttpParameters"
      ( \x ->
          ConnectionHttpParameters'
            Prelude.<$> ( x Prelude..:? "QueryStringParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "HeaderParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "BodyParameters"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConnectionHttpParameters

instance Prelude.NFData ConnectionHttpParameters

instance Prelude.ToJSON ConnectionHttpParameters where
  toJSON ConnectionHttpParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("QueryStringParameters" Prelude..=)
              Prelude.<$> queryStringParameters,
            ("HeaderParameters" Prelude..=)
              Prelude.<$> headerParameters,
            ("BodyParameters" Prelude..=)
              Prelude.<$> bodyParameters
          ]
      )
