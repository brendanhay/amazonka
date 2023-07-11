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
-- Module      : Amazonka.MediaTailor.Types.DefaultSegmentDeliveryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.DefaultSegmentDeliveryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The optional configuration for a server that serves segments. Use this
-- if you want the segment delivery server to be different from the source
-- location server. For example, you can configure your source location
-- server to be an origination server, such as MediaPackage, and the
-- segment delivery server to be a content delivery network (CDN), such as
-- CloudFront. If you don\'t specify a segment delivery server, then the
-- source location server is used.
--
-- /See:/ 'newDefaultSegmentDeliveryConfiguration' smart constructor.
data DefaultSegmentDeliveryConfiguration = DefaultSegmentDeliveryConfiguration'
  { -- | The hostname of the server that will be used to serve segments. This
    -- string must include the protocol, such as __https:\/\/__.
    baseUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultSegmentDeliveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseUrl', 'defaultSegmentDeliveryConfiguration_baseUrl' - The hostname of the server that will be used to serve segments. This
-- string must include the protocol, such as __https:\/\/__.
newDefaultSegmentDeliveryConfiguration ::
  DefaultSegmentDeliveryConfiguration
newDefaultSegmentDeliveryConfiguration =
  DefaultSegmentDeliveryConfiguration'
    { baseUrl =
        Prelude.Nothing
    }

-- | The hostname of the server that will be used to serve segments. This
-- string must include the protocol, such as __https:\/\/__.
defaultSegmentDeliveryConfiguration_baseUrl :: Lens.Lens' DefaultSegmentDeliveryConfiguration (Prelude.Maybe Prelude.Text)
defaultSegmentDeliveryConfiguration_baseUrl = Lens.lens (\DefaultSegmentDeliveryConfiguration' {baseUrl} -> baseUrl) (\s@DefaultSegmentDeliveryConfiguration' {} a -> s {baseUrl = a} :: DefaultSegmentDeliveryConfiguration)

instance
  Data.FromJSON
    DefaultSegmentDeliveryConfiguration
  where
  parseJSON =
    Data.withObject
      "DefaultSegmentDeliveryConfiguration"
      ( \x ->
          DefaultSegmentDeliveryConfiguration'
            Prelude.<$> (x Data..:? "BaseUrl")
      )

instance
  Prelude.Hashable
    DefaultSegmentDeliveryConfiguration
  where
  hashWithSalt
    _salt
    DefaultSegmentDeliveryConfiguration' {..} =
      _salt `Prelude.hashWithSalt` baseUrl

instance
  Prelude.NFData
    DefaultSegmentDeliveryConfiguration
  where
  rnf DefaultSegmentDeliveryConfiguration' {..} =
    Prelude.rnf baseUrl

instance
  Data.ToJSON
    DefaultSegmentDeliveryConfiguration
  where
  toJSON DefaultSegmentDeliveryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("BaseUrl" Data..=) Prelude.<$> baseUrl]
      )
