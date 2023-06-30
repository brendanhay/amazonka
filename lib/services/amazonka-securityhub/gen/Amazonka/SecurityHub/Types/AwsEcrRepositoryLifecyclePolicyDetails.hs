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
-- Module      : Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the lifecycle policy for the repository.
--
-- /See:/ 'newAwsEcrRepositoryLifecyclePolicyDetails' smart constructor.
data AwsEcrRepositoryLifecyclePolicyDetails = AwsEcrRepositoryLifecyclePolicyDetails'
  { -- | The text of the lifecycle policy.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account identifier that is associated with the
    -- registry that contains the repository.
    registryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrRepositoryLifecyclePolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecyclePolicyText', 'awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText' - The text of the lifecycle policy.
--
-- 'registryId', 'awsEcrRepositoryLifecyclePolicyDetails_registryId' - The Amazon Web Services account identifier that is associated with the
-- registry that contains the repository.
newAwsEcrRepositoryLifecyclePolicyDetails ::
  AwsEcrRepositoryLifecyclePolicyDetails
newAwsEcrRepositoryLifecyclePolicyDetails =
  AwsEcrRepositoryLifecyclePolicyDetails'
    { lifecyclePolicyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing
    }

-- | The text of the lifecycle policy.
awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText :: Lens.Lens' AwsEcrRepositoryLifecyclePolicyDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText = Lens.lens (\AwsEcrRepositoryLifecyclePolicyDetails' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@AwsEcrRepositoryLifecyclePolicyDetails' {} a -> s {lifecyclePolicyText = a} :: AwsEcrRepositoryLifecyclePolicyDetails)

-- | The Amazon Web Services account identifier that is associated with the
-- registry that contains the repository.
awsEcrRepositoryLifecyclePolicyDetails_registryId :: Lens.Lens' AwsEcrRepositoryLifecyclePolicyDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryLifecyclePolicyDetails_registryId = Lens.lens (\AwsEcrRepositoryLifecyclePolicyDetails' {registryId} -> registryId) (\s@AwsEcrRepositoryLifecyclePolicyDetails' {} a -> s {registryId = a} :: AwsEcrRepositoryLifecyclePolicyDetails)

instance
  Data.FromJSON
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcrRepositoryLifecyclePolicyDetails"
      ( \x ->
          AwsEcrRepositoryLifecyclePolicyDetails'
            Prelude.<$> (x Data..:? "LifecyclePolicyText")
            Prelude.<*> (x Data..:? "RegistryId")
      )

instance
  Prelude.Hashable
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  hashWithSalt
    _salt
    AwsEcrRepositoryLifecyclePolicyDetails' {..} =
      _salt
        `Prelude.hashWithSalt` lifecyclePolicyText
        `Prelude.hashWithSalt` registryId

instance
  Prelude.NFData
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  rnf AwsEcrRepositoryLifecyclePolicyDetails' {..} =
    Prelude.rnf lifecyclePolicyText
      `Prelude.seq` Prelude.rnf registryId

instance
  Data.ToJSON
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  toJSON AwsEcrRepositoryLifecyclePolicyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LifecyclePolicyText" Data..=)
              Prelude.<$> lifecyclePolicyText,
            ("RegistryId" Data..=) Prelude.<$> registryId
          ]
      )
