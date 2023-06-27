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
-- Module      : Amazonka.Firehose.Types.HttpEndpointDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the HTTP endpoint selected as the destination.
--
-- /See:/ 'newHttpEndpointDescription' smart constructor.
data HttpEndpointDescription = HttpEndpointDescription'
  { -- | The name of the HTTP endpoint selected as the destination.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL of the HTTP endpoint selected as the destination.
    url :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'httpEndpointDescription_name' - The name of the HTTP endpoint selected as the destination.
--
-- 'url', 'httpEndpointDescription_url' - The URL of the HTTP endpoint selected as the destination.
newHttpEndpointDescription ::
  HttpEndpointDescription
newHttpEndpointDescription =
  HttpEndpointDescription'
    { name = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The name of the HTTP endpoint selected as the destination.
httpEndpointDescription_name :: Lens.Lens' HttpEndpointDescription (Prelude.Maybe Prelude.Text)
httpEndpointDescription_name = Lens.lens (\HttpEndpointDescription' {name} -> name) (\s@HttpEndpointDescription' {} a -> s {name = a} :: HttpEndpointDescription)

-- | The URL of the HTTP endpoint selected as the destination.
httpEndpointDescription_url :: Lens.Lens' HttpEndpointDescription (Prelude.Maybe Prelude.Text)
httpEndpointDescription_url = Lens.lens (\HttpEndpointDescription' {url} -> url) (\s@HttpEndpointDescription' {} a -> s {url = a} :: HttpEndpointDescription) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON HttpEndpointDescription where
  parseJSON =
    Data.withObject
      "HttpEndpointDescription"
      ( \x ->
          HttpEndpointDescription'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable HttpEndpointDescription where
  hashWithSalt _salt HttpEndpointDescription' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url

instance Prelude.NFData HttpEndpointDescription where
  rnf HttpEndpointDescription' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf url
