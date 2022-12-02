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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorDataSharingPreference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorDataSharingPreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Allows you to opt in or opt out to share data with Rekognition to
-- improve model performance. You can choose this option at the account
-- level or on a per-stream basis. Note that if you opt out at the account
-- level this setting is ignored on individual streams.
--
-- /See:/ 'newStreamProcessorDataSharingPreference' smart constructor.
data StreamProcessorDataSharingPreference = StreamProcessorDataSharingPreference'
  { -- | If this option is set to true, you choose to share data with Rekognition
    -- to improve model performance.
    optIn :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessorDataSharingPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optIn', 'streamProcessorDataSharingPreference_optIn' - If this option is set to true, you choose to share data with Rekognition
-- to improve model performance.
newStreamProcessorDataSharingPreference ::
  -- | 'optIn'
  Prelude.Bool ->
  StreamProcessorDataSharingPreference
newStreamProcessorDataSharingPreference pOptIn_ =
  StreamProcessorDataSharingPreference'
    { optIn =
        pOptIn_
    }

-- | If this option is set to true, you choose to share data with Rekognition
-- to improve model performance.
streamProcessorDataSharingPreference_optIn :: Lens.Lens' StreamProcessorDataSharingPreference Prelude.Bool
streamProcessorDataSharingPreference_optIn = Lens.lens (\StreamProcessorDataSharingPreference' {optIn} -> optIn) (\s@StreamProcessorDataSharingPreference' {} a -> s {optIn = a} :: StreamProcessorDataSharingPreference)

instance
  Data.FromJSON
    StreamProcessorDataSharingPreference
  where
  parseJSON =
    Data.withObject
      "StreamProcessorDataSharingPreference"
      ( \x ->
          StreamProcessorDataSharingPreference'
            Prelude.<$> (x Data..: "OptIn")
      )

instance
  Prelude.Hashable
    StreamProcessorDataSharingPreference
  where
  hashWithSalt
    _salt
    StreamProcessorDataSharingPreference' {..} =
      _salt `Prelude.hashWithSalt` optIn

instance
  Prelude.NFData
    StreamProcessorDataSharingPreference
  where
  rnf StreamProcessorDataSharingPreference' {..} =
    Prelude.rnf optIn

instance
  Data.ToJSON
    StreamProcessorDataSharingPreference
  where
  toJSON StreamProcessorDataSharingPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("OptIn" Data..= optIn)]
      )
