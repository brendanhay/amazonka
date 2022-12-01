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
-- Module      : Amazonka.Lightsail.Types.BucketState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.BucketState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of an Amazon Lightsail bucket.
--
-- /See:/ 'newBucketState' smart constructor.
data BucketState = BucketState'
  { -- | A message that describes the state of the bucket.
    message :: Prelude.Maybe Prelude.Text,
    -- | The state code of the bucket.
    --
    -- The following codes are possible:
    --
    -- -   @OK@ - The bucket is in a running state.
    --
    -- -   @Unknown@ - Creation of the bucket might have timed-out. You might
    --     want to delete the bucket and create a new one.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'bucketState_message' - A message that describes the state of the bucket.
--
-- 'code', 'bucketState_code' - The state code of the bucket.
--
-- The following codes are possible:
--
-- -   @OK@ - The bucket is in a running state.
--
-- -   @Unknown@ - Creation of the bucket might have timed-out. You might
--     want to delete the bucket and create a new one.
newBucketState ::
  BucketState
newBucketState =
  BucketState'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message that describes the state of the bucket.
bucketState_message :: Lens.Lens' BucketState (Prelude.Maybe Prelude.Text)
bucketState_message = Lens.lens (\BucketState' {message} -> message) (\s@BucketState' {} a -> s {message = a} :: BucketState)

-- | The state code of the bucket.
--
-- The following codes are possible:
--
-- -   @OK@ - The bucket is in a running state.
--
-- -   @Unknown@ - Creation of the bucket might have timed-out. You might
--     want to delete the bucket and create a new one.
bucketState_code :: Lens.Lens' BucketState (Prelude.Maybe Prelude.Text)
bucketState_code = Lens.lens (\BucketState' {code} -> code) (\s@BucketState' {} a -> s {code = a} :: BucketState)

instance Core.FromJSON BucketState where
  parseJSON =
    Core.withObject
      "BucketState"
      ( \x ->
          BucketState'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "code")
      )

instance Prelude.Hashable BucketState where
  hashWithSalt _salt BucketState' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData BucketState where
  rnf BucketState' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
