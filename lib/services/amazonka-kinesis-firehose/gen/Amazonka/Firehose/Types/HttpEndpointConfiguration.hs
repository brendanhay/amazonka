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
-- Module      : Amazonka.Firehose.Types.HttpEndpointConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of the HTTP endpoint to which Kinesis
-- Firehose delivers data.
--
-- /See:/ 'newHttpEndpointConfiguration' smart constructor.
data HttpEndpointConfiguration = HttpEndpointConfiguration'
  { -- | The name of the HTTP endpoint selected as the destination.
    name :: Prelude.Maybe Prelude.Text,
    -- | The access key required for Kinesis Firehose to authenticate with the
    -- HTTP endpoint selected as the destination.
    accessKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The URL of the HTTP endpoint selected as the destination.
    --
    -- If you choose an HTTP endpoint as your destination, review and follow
    -- the instructions in the
    -- <https://docs.aws.amazon.com/firehose/latest/dev/httpdeliveryrequestresponse.html Appendix - HTTP Endpoint Delivery Request and Response Specifications>.
    url :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'httpEndpointConfiguration_name' - The name of the HTTP endpoint selected as the destination.
--
-- 'accessKey', 'httpEndpointConfiguration_accessKey' - The access key required for Kinesis Firehose to authenticate with the
-- HTTP endpoint selected as the destination.
--
-- 'url', 'httpEndpointConfiguration_url' - The URL of the HTTP endpoint selected as the destination.
--
-- If you choose an HTTP endpoint as your destination, review and follow
-- the instructions in the
-- <https://docs.aws.amazon.com/firehose/latest/dev/httpdeliveryrequestresponse.html Appendix - HTTP Endpoint Delivery Request and Response Specifications>.
newHttpEndpointConfiguration ::
  -- | 'url'
  Prelude.Text ->
  HttpEndpointConfiguration
newHttpEndpointConfiguration pUrl_ =
  HttpEndpointConfiguration'
    { name = Prelude.Nothing,
      accessKey = Prelude.Nothing,
      url = Data._Sensitive Lens.# pUrl_
    }

-- | The name of the HTTP endpoint selected as the destination.
httpEndpointConfiguration_name :: Lens.Lens' HttpEndpointConfiguration (Prelude.Maybe Prelude.Text)
httpEndpointConfiguration_name = Lens.lens (\HttpEndpointConfiguration' {name} -> name) (\s@HttpEndpointConfiguration' {} a -> s {name = a} :: HttpEndpointConfiguration)

-- | The access key required for Kinesis Firehose to authenticate with the
-- HTTP endpoint selected as the destination.
httpEndpointConfiguration_accessKey :: Lens.Lens' HttpEndpointConfiguration (Prelude.Maybe Prelude.Text)
httpEndpointConfiguration_accessKey = Lens.lens (\HttpEndpointConfiguration' {accessKey} -> accessKey) (\s@HttpEndpointConfiguration' {} a -> s {accessKey = a} :: HttpEndpointConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The URL of the HTTP endpoint selected as the destination.
--
-- If you choose an HTTP endpoint as your destination, review and follow
-- the instructions in the
-- <https://docs.aws.amazon.com/firehose/latest/dev/httpdeliveryrequestresponse.html Appendix - HTTP Endpoint Delivery Request and Response Specifications>.
httpEndpointConfiguration_url :: Lens.Lens' HttpEndpointConfiguration Prelude.Text
httpEndpointConfiguration_url = Lens.lens (\HttpEndpointConfiguration' {url} -> url) (\s@HttpEndpointConfiguration' {} a -> s {url = a} :: HttpEndpointConfiguration) Prelude.. Data._Sensitive

instance Prelude.Hashable HttpEndpointConfiguration where
  hashWithSalt _salt HttpEndpointConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` accessKey
      `Prelude.hashWithSalt` url

instance Prelude.NFData HttpEndpointConfiguration where
  rnf HttpEndpointConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf accessKey
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON HttpEndpointConfiguration where
  toJSON HttpEndpointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("AccessKey" Data..=) Prelude.<$> accessKey,
            Prelude.Just ("Url" Data..= url)
          ]
      )
