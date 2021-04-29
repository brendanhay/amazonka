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
-- Module      : Network.AWS.DeviceFarm.Types.Sample
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Sample where

import Network.AWS.DeviceFarm.Types.SampleType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a sample of performance data.
--
-- /See:/ 'newSample' smart constructor.
data Sample = Sample'
  { -- | The sample\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The presigned Amazon S3 URL that can be used with a GET request to
    -- download the sample\'s file.
    url :: Prelude.Maybe Prelude.Text,
    -- | The sample\'s type.
    --
    -- Must be one of the following values:
    --
    -- -   CPU: A CPU sample type. This is expressed as the app processing CPU
    --     time (including child processes) as reported by process, as a
    --     percentage.
    --
    -- -   MEMORY: A memory usage sample type. This is expressed as the total
    --     proportional set size of an app process, in kilobytes.
    --
    -- -   NATIVE_AVG_DRAWTIME
    --
    -- -   NATIVE_FPS
    --
    -- -   NATIVE_FRAMES
    --
    -- -   NATIVE_MAX_DRAWTIME
    --
    -- -   NATIVE_MIN_DRAWTIME
    --
    -- -   OPENGL_AVG_DRAWTIME
    --
    -- -   OPENGL_FPS
    --
    -- -   OPENGL_FRAMES
    --
    -- -   OPENGL_MAX_DRAWTIME
    --
    -- -   OPENGL_MIN_DRAWTIME
    --
    -- -   RX
    --
    -- -   RX_RATE: The total number of bytes per second (TCP and UDP) that are
    --     sent, by app process.
    --
    -- -   THREADS: A threads sample type. This is expressed as the total
    --     number of threads per app process.
    --
    -- -   TX
    --
    -- -   TX_RATE: The total number of bytes per second (TCP and UDP) that are
    --     received, by app process.
    type' :: Prelude.Maybe SampleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Sample' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'sample_arn' - The sample\'s ARN.
--
-- 'url', 'sample_url' - The presigned Amazon S3 URL that can be used with a GET request to
-- download the sample\'s file.
--
-- 'type'', 'sample_type' - The sample\'s type.
--
-- Must be one of the following values:
--
-- -   CPU: A CPU sample type. This is expressed as the app processing CPU
--     time (including child processes) as reported by process, as a
--     percentage.
--
-- -   MEMORY: A memory usage sample type. This is expressed as the total
--     proportional set size of an app process, in kilobytes.
--
-- -   NATIVE_AVG_DRAWTIME
--
-- -   NATIVE_FPS
--
-- -   NATIVE_FRAMES
--
-- -   NATIVE_MAX_DRAWTIME
--
-- -   NATIVE_MIN_DRAWTIME
--
-- -   OPENGL_AVG_DRAWTIME
--
-- -   OPENGL_FPS
--
-- -   OPENGL_FRAMES
--
-- -   OPENGL_MAX_DRAWTIME
--
-- -   OPENGL_MIN_DRAWTIME
--
-- -   RX
--
-- -   RX_RATE: The total number of bytes per second (TCP and UDP) that are
--     sent, by app process.
--
-- -   THREADS: A threads sample type. This is expressed as the total
--     number of threads per app process.
--
-- -   TX
--
-- -   TX_RATE: The total number of bytes per second (TCP and UDP) that are
--     received, by app process.
newSample ::
  Sample
newSample =
  Sample'
    { arn = Prelude.Nothing,
      url = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The sample\'s ARN.
sample_arn :: Lens.Lens' Sample (Prelude.Maybe Prelude.Text)
sample_arn = Lens.lens (\Sample' {arn} -> arn) (\s@Sample' {} a -> s {arn = a} :: Sample)

-- | The presigned Amazon S3 URL that can be used with a GET request to
-- download the sample\'s file.
sample_url :: Lens.Lens' Sample (Prelude.Maybe Prelude.Text)
sample_url = Lens.lens (\Sample' {url} -> url) (\s@Sample' {} a -> s {url = a} :: Sample)

-- | The sample\'s type.
--
-- Must be one of the following values:
--
-- -   CPU: A CPU sample type. This is expressed as the app processing CPU
--     time (including child processes) as reported by process, as a
--     percentage.
--
-- -   MEMORY: A memory usage sample type. This is expressed as the total
--     proportional set size of an app process, in kilobytes.
--
-- -   NATIVE_AVG_DRAWTIME
--
-- -   NATIVE_FPS
--
-- -   NATIVE_FRAMES
--
-- -   NATIVE_MAX_DRAWTIME
--
-- -   NATIVE_MIN_DRAWTIME
--
-- -   OPENGL_AVG_DRAWTIME
--
-- -   OPENGL_FPS
--
-- -   OPENGL_FRAMES
--
-- -   OPENGL_MAX_DRAWTIME
--
-- -   OPENGL_MIN_DRAWTIME
--
-- -   RX
--
-- -   RX_RATE: The total number of bytes per second (TCP and UDP) that are
--     sent, by app process.
--
-- -   THREADS: A threads sample type. This is expressed as the total
--     number of threads per app process.
--
-- -   TX
--
-- -   TX_RATE: The total number of bytes per second (TCP and UDP) that are
--     received, by app process.
sample_type :: Lens.Lens' Sample (Prelude.Maybe SampleType)
sample_type = Lens.lens (\Sample' {type'} -> type') (\s@Sample' {} a -> s {type' = a} :: Sample)

instance Prelude.FromJSON Sample where
  parseJSON =
    Prelude.withObject
      "Sample"
      ( \x ->
          Sample'
            Prelude.<$> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "url")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable Sample

instance Prelude.NFData Sample
