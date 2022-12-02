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
-- Module      : Amazonka.MediaTailor.Types.SegmentDeliveryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SegmentDeliveryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The segment delivery configuration settings.
--
-- /See:/ 'newSegmentDeliveryConfiguration' smart constructor.
data SegmentDeliveryConfiguration = SegmentDeliveryConfiguration'
  { -- | The base URL of the host or path of the segment delivery server that
    -- you\'re using to serve segments. This is typically a content delivery
    -- network (CDN). The URL can be absolute or relative. To use an absolute
    -- URL include the protocol, such as @https:\/\/example.com\/some\/path@.
    -- To use a relative URL specify the relative path, such as
    -- @\/some\/path*@.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier used to distinguish between multiple segment
    -- delivery configurations in a source location.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentDeliveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseUrl', 'segmentDeliveryConfiguration_baseUrl' - The base URL of the host or path of the segment delivery server that
-- you\'re using to serve segments. This is typically a content delivery
-- network (CDN). The URL can be absolute or relative. To use an absolute
-- URL include the protocol, such as @https:\/\/example.com\/some\/path@.
-- To use a relative URL specify the relative path, such as
-- @\/some\/path*@.
--
-- 'name', 'segmentDeliveryConfiguration_name' - A unique identifier used to distinguish between multiple segment
-- delivery configurations in a source location.
newSegmentDeliveryConfiguration ::
  SegmentDeliveryConfiguration
newSegmentDeliveryConfiguration =
  SegmentDeliveryConfiguration'
    { baseUrl =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The base URL of the host or path of the segment delivery server that
-- you\'re using to serve segments. This is typically a content delivery
-- network (CDN). The URL can be absolute or relative. To use an absolute
-- URL include the protocol, such as @https:\/\/example.com\/some\/path@.
-- To use a relative URL specify the relative path, such as
-- @\/some\/path*@.
segmentDeliveryConfiguration_baseUrl :: Lens.Lens' SegmentDeliveryConfiguration (Prelude.Maybe Prelude.Text)
segmentDeliveryConfiguration_baseUrl = Lens.lens (\SegmentDeliveryConfiguration' {baseUrl} -> baseUrl) (\s@SegmentDeliveryConfiguration' {} a -> s {baseUrl = a} :: SegmentDeliveryConfiguration)

-- | A unique identifier used to distinguish between multiple segment
-- delivery configurations in a source location.
segmentDeliveryConfiguration_name :: Lens.Lens' SegmentDeliveryConfiguration (Prelude.Maybe Prelude.Text)
segmentDeliveryConfiguration_name = Lens.lens (\SegmentDeliveryConfiguration' {name} -> name) (\s@SegmentDeliveryConfiguration' {} a -> s {name = a} :: SegmentDeliveryConfiguration)

instance Data.FromJSON SegmentDeliveryConfiguration where
  parseJSON =
    Data.withObject
      "SegmentDeliveryConfiguration"
      ( \x ->
          SegmentDeliveryConfiguration'
            Prelude.<$> (x Data..:? "BaseUrl")
            Prelude.<*> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    SegmentDeliveryConfiguration
  where
  hashWithSalt _salt SegmentDeliveryConfiguration' {..} =
    _salt `Prelude.hashWithSalt` baseUrl
      `Prelude.hashWithSalt` name

instance Prelude.NFData SegmentDeliveryConfiguration where
  rnf SegmentDeliveryConfiguration' {..} =
    Prelude.rnf baseUrl `Prelude.seq` Prelude.rnf name

instance Data.ToJSON SegmentDeliveryConfiguration where
  toJSON SegmentDeliveryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseUrl" Data..=) Prelude.<$> baseUrl,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
