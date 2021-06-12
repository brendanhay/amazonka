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
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ThirdPartyJobDetails where

import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details of a job sent in response to a @GetThirdPartyJobDetails@
-- request.
--
-- /See:/ 'newThirdPartyJobDetails' smart constructor.
data ThirdPartyJobDetails = ThirdPartyJobDetails'
  { -- | A system-generated random number that AWS CodePipeline uses to ensure
    -- that the job is being worked on by only one job worker. Use this number
    -- in an AcknowledgeThirdPartyJob request.
    nonce :: Core.Maybe Core.Text,
    -- | The data to be returned by the third party job worker.
    data' :: Core.Maybe ThirdPartyJobData,
    -- | The identifier used to identify the job details in AWS CodePipeline.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThirdPartyJobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonce', 'thirdPartyJobDetails_nonce' - A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Use this number
-- in an AcknowledgeThirdPartyJob request.
--
-- 'data'', 'thirdPartyJobDetails_data' - The data to be returned by the third party job worker.
--
-- 'id', 'thirdPartyJobDetails_id' - The identifier used to identify the job details in AWS CodePipeline.
newThirdPartyJobDetails ::
  ThirdPartyJobDetails
newThirdPartyJobDetails =
  ThirdPartyJobDetails'
    { nonce = Core.Nothing,
      data' = Core.Nothing,
      id = Core.Nothing
    }

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Use this number
-- in an AcknowledgeThirdPartyJob request.
thirdPartyJobDetails_nonce :: Lens.Lens' ThirdPartyJobDetails (Core.Maybe Core.Text)
thirdPartyJobDetails_nonce = Lens.lens (\ThirdPartyJobDetails' {nonce} -> nonce) (\s@ThirdPartyJobDetails' {} a -> s {nonce = a} :: ThirdPartyJobDetails)

-- | The data to be returned by the third party job worker.
thirdPartyJobDetails_data :: Lens.Lens' ThirdPartyJobDetails (Core.Maybe ThirdPartyJobData)
thirdPartyJobDetails_data = Lens.lens (\ThirdPartyJobDetails' {data'} -> data') (\s@ThirdPartyJobDetails' {} a -> s {data' = a} :: ThirdPartyJobDetails)

-- | The identifier used to identify the job details in AWS CodePipeline.
thirdPartyJobDetails_id :: Lens.Lens' ThirdPartyJobDetails (Core.Maybe Core.Text)
thirdPartyJobDetails_id = Lens.lens (\ThirdPartyJobDetails' {id} -> id) (\s@ThirdPartyJobDetails' {} a -> s {id = a} :: ThirdPartyJobDetails)

instance Core.FromJSON ThirdPartyJobDetails where
  parseJSON =
    Core.withObject
      "ThirdPartyJobDetails"
      ( \x ->
          ThirdPartyJobDetails'
            Core.<$> (x Core..:? "nonce")
            Core.<*> (x Core..:? "data")
            Core.<*> (x Core..:? "id")
      )

instance Core.Hashable ThirdPartyJobDetails

instance Core.NFData ThirdPartyJobDetails
