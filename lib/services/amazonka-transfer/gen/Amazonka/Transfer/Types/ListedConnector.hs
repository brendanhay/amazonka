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
-- Module      : Amazonka.Transfer.Types.ListedConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedConnector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns details of the connector that is specified.
--
-- /See:/ 'newListedConnector' smart constructor.
data ListedConnector = ListedConnector'
  { -- | The unique identifier for the connector.
    connectorId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the specified connector.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The URL of the partner\'s AS2 endpoint.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorId', 'listedConnector_connectorId' - The unique identifier for the connector.
--
-- 'arn', 'listedConnector_arn' - The Amazon Resource Name (ARN) of the specified connector.
--
-- 'url', 'listedConnector_url' - The URL of the partner\'s AS2 endpoint.
newListedConnector ::
  ListedConnector
newListedConnector =
  ListedConnector'
    { connectorId = Prelude.Nothing,
      arn = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The unique identifier for the connector.
listedConnector_connectorId :: Lens.Lens' ListedConnector (Prelude.Maybe Prelude.Text)
listedConnector_connectorId = Lens.lens (\ListedConnector' {connectorId} -> connectorId) (\s@ListedConnector' {} a -> s {connectorId = a} :: ListedConnector)

-- | The Amazon Resource Name (ARN) of the specified connector.
listedConnector_arn :: Lens.Lens' ListedConnector (Prelude.Maybe Prelude.Text)
listedConnector_arn = Lens.lens (\ListedConnector' {arn} -> arn) (\s@ListedConnector' {} a -> s {arn = a} :: ListedConnector)

-- | The URL of the partner\'s AS2 endpoint.
listedConnector_url :: Lens.Lens' ListedConnector (Prelude.Maybe Prelude.Text)
listedConnector_url = Lens.lens (\ListedConnector' {url} -> url) (\s@ListedConnector' {} a -> s {url = a} :: ListedConnector)

instance Data.FromJSON ListedConnector where
  parseJSON =
    Data.withObject
      "ListedConnector"
      ( \x ->
          ListedConnector'
            Prelude.<$> (x Data..:? "ConnectorId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable ListedConnector where
  hashWithSalt _salt ListedConnector' {..} =
    _salt `Prelude.hashWithSalt` connectorId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` url

instance Prelude.NFData ListedConnector where
  rnf ListedConnector' {..} =
    Prelude.rnf connectorId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf url
