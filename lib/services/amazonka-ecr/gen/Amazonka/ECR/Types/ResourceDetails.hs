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
-- Module      : Amazonka.ECR.Types.ResourceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.AwsEcrContainerImageDetails
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the resource involved in the finding.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | An object that contains details about the Amazon ECR container image
    -- involved in the finding.
    awsEcrContainerImage :: Prelude.Maybe AwsEcrContainerImageDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsEcrContainerImage', 'resourceDetails_awsEcrContainerImage' - An object that contains details about the Amazon ECR container image
-- involved in the finding.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { awsEcrContainerImage =
        Prelude.Nothing
    }

-- | An object that contains details about the Amazon ECR container image
-- involved in the finding.
resourceDetails_awsEcrContainerImage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcrContainerImageDetails)
resourceDetails_awsEcrContainerImage = Lens.lens (\ResourceDetails' {awsEcrContainerImage} -> awsEcrContainerImage) (\s@ResourceDetails' {} a -> s {awsEcrContainerImage = a} :: ResourceDetails)

instance Data.FromJSON ResourceDetails where
  parseJSON =
    Data.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Data..:? "awsEcrContainerImage")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt `Prelude.hashWithSalt` awsEcrContainerImage

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf awsEcrContainerImage
