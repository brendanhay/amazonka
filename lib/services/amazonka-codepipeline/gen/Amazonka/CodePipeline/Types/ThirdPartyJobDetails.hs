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
-- Module      : Amazonka.CodePipeline.Types.ThirdPartyJobDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ThirdPartyJobDetails where

import Amazonka.CodePipeline.Types.ThirdPartyJobData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a job sent in response to a @GetThirdPartyJobDetails@
-- request.
--
-- /See:/ 'newThirdPartyJobDetails' smart constructor.
data ThirdPartyJobDetails = ThirdPartyJobDetails'
  { -- | The data to be returned by the third party job worker.
    data' :: Prelude.Maybe ThirdPartyJobData,
    -- | The identifier used to identify the job details in AWS CodePipeline.
    id :: Prelude.Maybe Prelude.Text,
    -- | A system-generated random number that AWS CodePipeline uses to ensure
    -- that the job is being worked on by only one job worker. Use this number
    -- in an AcknowledgeThirdPartyJob request.
    nonce :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThirdPartyJobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'thirdPartyJobDetails_data' - The data to be returned by the third party job worker.
--
-- 'id', 'thirdPartyJobDetails_id' - The identifier used to identify the job details in AWS CodePipeline.
--
-- 'nonce', 'thirdPartyJobDetails_nonce' - A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Use this number
-- in an AcknowledgeThirdPartyJob request.
newThirdPartyJobDetails ::
  ThirdPartyJobDetails
newThirdPartyJobDetails =
  ThirdPartyJobDetails'
    { data' = Prelude.Nothing,
      id = Prelude.Nothing,
      nonce = Prelude.Nothing
    }

-- | The data to be returned by the third party job worker.
thirdPartyJobDetails_data :: Lens.Lens' ThirdPartyJobDetails (Prelude.Maybe ThirdPartyJobData)
thirdPartyJobDetails_data = Lens.lens (\ThirdPartyJobDetails' {data'} -> data') (\s@ThirdPartyJobDetails' {} a -> s {data' = a} :: ThirdPartyJobDetails)

-- | The identifier used to identify the job details in AWS CodePipeline.
thirdPartyJobDetails_id :: Lens.Lens' ThirdPartyJobDetails (Prelude.Maybe Prelude.Text)
thirdPartyJobDetails_id = Lens.lens (\ThirdPartyJobDetails' {id} -> id) (\s@ThirdPartyJobDetails' {} a -> s {id = a} :: ThirdPartyJobDetails)

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Use this number
-- in an AcknowledgeThirdPartyJob request.
thirdPartyJobDetails_nonce :: Lens.Lens' ThirdPartyJobDetails (Prelude.Maybe Prelude.Text)
thirdPartyJobDetails_nonce = Lens.lens (\ThirdPartyJobDetails' {nonce} -> nonce) (\s@ThirdPartyJobDetails' {} a -> s {nonce = a} :: ThirdPartyJobDetails)

instance Data.FromJSON ThirdPartyJobDetails where
  parseJSON =
    Data.withObject
      "ThirdPartyJobDetails"
      ( \x ->
          ThirdPartyJobDetails'
            Prelude.<$> (x Data..:? "data")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "nonce")
      )

instance Prelude.Hashable ThirdPartyJobDetails where
  hashWithSalt _salt ThirdPartyJobDetails' {..} =
    _salt `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` nonce

instance Prelude.NFData ThirdPartyJobDetails where
  rnf ThirdPartyJobDetails' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf nonce
