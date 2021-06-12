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
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputUrl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputUrl where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.S3OutputUrl

-- | The URL of S3 bucket where you want to store the results of this
-- request.
--
-- /See:/ 'newInstanceAssociationOutputUrl' smart constructor.
data InstanceAssociationOutputUrl = InstanceAssociationOutputUrl'
  { -- | The URL of S3 bucket where you want to store the results of this
    -- request.
    s3OutputUrl :: Core.Maybe S3OutputUrl
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceAssociationOutputUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputUrl', 'instanceAssociationOutputUrl_s3OutputUrl' - The URL of S3 bucket where you want to store the results of this
-- request.
newInstanceAssociationOutputUrl ::
  InstanceAssociationOutputUrl
newInstanceAssociationOutputUrl =
  InstanceAssociationOutputUrl'
    { s3OutputUrl =
        Core.Nothing
    }

-- | The URL of S3 bucket where you want to store the results of this
-- request.
instanceAssociationOutputUrl_s3OutputUrl :: Lens.Lens' InstanceAssociationOutputUrl (Core.Maybe S3OutputUrl)
instanceAssociationOutputUrl_s3OutputUrl = Lens.lens (\InstanceAssociationOutputUrl' {s3OutputUrl} -> s3OutputUrl) (\s@InstanceAssociationOutputUrl' {} a -> s {s3OutputUrl = a} :: InstanceAssociationOutputUrl)

instance Core.FromJSON InstanceAssociationOutputUrl where
  parseJSON =
    Core.withObject
      "InstanceAssociationOutputUrl"
      ( \x ->
          InstanceAssociationOutputUrl'
            Core.<$> (x Core..:? "S3OutputUrl")
      )

instance Core.Hashable InstanceAssociationOutputUrl

instance Core.NFData InstanceAssociationOutputUrl
