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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of the HTTP endpoint to which Kinesis
-- Firehose delivers data.
--
-- /See:/ 'newHttpEndpointConfiguration' smart constructor.
data HttpEndpointConfiguration = HttpEndpointConfiguration'
  { -- | The access key required for Kinesis Firehose to authenticate with the
    -- HTTP endpoint selected as the destination.
    accessKey :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name of the HTTP endpoint selected as the destination.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL of the HTTP endpoint selected as the destination.
    url :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKey', 'httpEndpointConfiguration_accessKey' - The access key required for Kinesis Firehose to authenticate with the
-- HTTP endpoint selected as the destination.
--
-- 'name', 'httpEndpointConfiguration_name' - The name of the HTTP endpoint selected as the destination.
--
-- 'url', 'httpEndpointConfiguration_url' - The URL of the HTTP endpoint selected as the destination.
newHttpEndpointConfiguration ::
  -- | 'url'
  Prelude.Text ->
  HttpEndpointConfiguration
newHttpEndpointConfiguration pUrl_ =
  HttpEndpointConfiguration'
    { accessKey =
        Prelude.Nothing,
      name = Prelude.Nothing,
      url = Prelude._Sensitive Lens.# pUrl_
    }

-- | The access key required for Kinesis Firehose to authenticate with the
-- HTTP endpoint selected as the destination.
httpEndpointConfiguration_accessKey :: Lens.Lens' HttpEndpointConfiguration (Prelude.Maybe Prelude.Text)
httpEndpointConfiguration_accessKey = Lens.lens (\HttpEndpointConfiguration' {accessKey} -> accessKey) (\s@HttpEndpointConfiguration' {} a -> s {accessKey = a} :: HttpEndpointConfiguration) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the HTTP endpoint selected as the destination.
httpEndpointConfiguration_name :: Lens.Lens' HttpEndpointConfiguration (Prelude.Maybe Prelude.Text)
httpEndpointConfiguration_name = Lens.lens (\HttpEndpointConfiguration' {name} -> name) (\s@HttpEndpointConfiguration' {} a -> s {name = a} :: HttpEndpointConfiguration)

-- | The URL of the HTTP endpoint selected as the destination.
httpEndpointConfiguration_url :: Lens.Lens' HttpEndpointConfiguration Prelude.Text
httpEndpointConfiguration_url = Lens.lens (\HttpEndpointConfiguration' {url} -> url) (\s@HttpEndpointConfiguration' {} a -> s {url = a} :: HttpEndpointConfiguration) Prelude.. Prelude._Sensitive

instance Prelude.Hashable HttpEndpointConfiguration

instance Prelude.NFData HttpEndpointConfiguration

instance Prelude.ToJSON HttpEndpointConfiguration where
  toJSON HttpEndpointConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccessKey" Prelude..=) Prelude.<$> accessKey,
            ("Name" Prelude..=) Prelude.<$> name,
            Prelude.Just ("Url" Prelude..= url)
          ]
      )
